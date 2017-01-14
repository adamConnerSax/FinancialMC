{-# LANGUAGE FlexibleContexts #-}
module BenchMapLike
       (
         benchMapLikeIO
       ) where

import Criterion
import BenchUtils

import FinancialMC.Base (IsMap(..),IndexedList(..))

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import Control.DeepSeq (NFData)

type MapK = T.Text
type MapV = Double

mapElts = [(T.pack "NameA",5),(T.pack "NameB",2),(T.pack "NameC",7),(T.pack "NameD",5),(T.pack "NameE",2),(T.pack "NameF",7)] 

f::Double->Maybe Double
f x = Just $ 2.0 * x

ilFromList::Ord k=>[(k,v)]->IndexedList k v
ilFromList l = mFromList l 

benchFromList = bgroup "fromList"
                [ bench "Map" $ nf M.fromList mapElts
                , bench "HashMap" $ nf HM.fromList mapElts
                , bench "IndexedList" $ nf ilFromList mapElts
                ]



benchTraverseF::(Traversable a,NFData (a Double))=>String->([(MapK,MapV)]->a Double)->[(MapK,MapV)]->Benchmark
benchTraverseF name builder elts = let m = builder elts in m `seq` bench name $ nf (traverse f) m

benchTraverse = bgroup "Traverse"
                [ benchTraverseF "map" M.fromList mapElts
                , benchTraverseF "hashMap" HM.fromList mapElts
                , benchTraverseF "IndexedList" ilFromList mapElts
                ]

benchInsertF::(Traversable a,NFData (a MapV),IsMap MapK MapV (a MapV))=>String->([(MapK,MapV)]->a MapV)->[(MapK,MapV)]->Benchmark
benchInsertF name builder elts = let m = builder elts in m `seq` bench name $ nf (mInsert (T.pack "NameAA") (12 :: Double)) m

benchInsert = bgroup "Insert"
                [ benchInsertF "map" M.fromList mapElts
                , benchInsertF "hashMap" HM.fromList mapElts
                , benchInsertF "IndexedList" ilFromList mapElts
                ]



benchMapLikeIO :: IO Benchmark
benchMapLikeIO = return $ bgroup "MapLike" [benchFromList,benchInsert,benchTraverse]
