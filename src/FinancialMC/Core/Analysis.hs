{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module FinancialMC.Core.Analysis
         (
           DatedMoneyValue (..)
         , HasDatedMoneyValue (..)
         , SimHistories (..)
         , HasSimHistories (..)
         , DatedSummaryWithReturns (..)
         , summariesToHistogram
         , nwHistFromSummaries
         , historiesFromSummaries
         , addReturns
         , analyzeBankruptcies
         ) where

import           FinancialMC.Core.Asset           (IsAsset)
import           FinancialMC.Core.Flow            (IsFlow)
import           FinancialMC.Core.LifeEvent       (LifeEventConverters)
import           FinancialMC.Core.MCState         (DatedSummary (..),
                                                   FSSummary (..),
                                                   HasDatedSummary (..),
                                                   HasFSSummary (..),
                                                   netWorthBreakout)
import           FinancialMC.Core.MoneyValue      (HasMoneyValue (..),
                                                   MoneyValue (..))
import qualified FinancialMC.Core.MoneyValueOps   as MV

import           FinancialMC.Core.Engine          (EngineC,
                                                   HasPathSummaryAndSeed (..),
                                                   PathSummaryAndSeed (..),
                                                   RandomSeed, execOnePathPure)

--import FinancialMC.Core.Flow (FlowDirection(..))
import           FinancialMC.Core.FinancialStates (FinEnv, HasFinEnv (..))
import           FinancialMC.Core.MCState         (CombinedState,
                                                   HasCombinedState (..),
                                                   HasMCState (..),
                                                   PathSummary (..), grossFlows,
                                                   netWorth)
import           FinancialMC.Core.Utilities       (Year)

import           Control.Exception                (SomeException)
import           Control.Lens                     (makeClassy, view, (&), (.~),
                                                   (^.), _1, _2)
import           Control.Parallel.Strategies      (parList, rseq, using)
import           Data.List                        (foldl', sort, sortBy)
import qualified Data.Map                         as M
import           Data.Ord                         (comparing)
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic.Base         as VGB
import           Data.Word                        (Word64)
import           Statistics.Sample.Histogram      (histogram)

psasToNumber :: PathSummaryAndSeed -> Double
psasToNumber (PathSummaryAndSeed (FinalNW mv) _) = mv ^. mAmount
psasToNumber (PathSummaryAndSeed (ZeroNW _) _)   = 0

sortSummaries::[PathSummaryAndSeed] -> [PathSummaryAndSeed]
sortSummaries = sortBy (\x y-> compare (psasToNumber x) (psasToNumber y))

{-
getHistory::CombinedState a fl le ru->V.Vector DatedSummary
getHistory cs = cs ^. csMC.mcsHistory
-}

qIndices::Int->Int->[Int]
qIndices len quantiles = map (\n -> (2*n - 1) * len `div` (2 * quantiles)) [1..quantiles]

qSubSet::[Int]->[a]->[a]
qSubSet is xs = map (\n -> xs !! n) is

data DatedMoneyValue = DatedMoneyValue { _dmYear :: !Year, _dmValue :: !MoneyValue } deriving (Show)

data SimHistories = SimHistories { _simQuartilesNW :: [V.Vector DatedMoneyValue]
                                 , _simMedianDetails :: V.Vector DatedSummary
                                 } deriving (Show)

makeClassy ''DatedMoneyValue
makeClassy ''SimHistories

historiesFromSummaries::EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> [PathSummaryAndSeed]
  -> (FinEnv rm,CombinedState a fl le ru)
  -> Bool -- single threaded ?
  -> Int -- number of quantiles
  -> Int -- years per path
  -> Either SomeException SimHistories
historiesFromSummaries convertLE summaries (fe0,cs0) singleThreaded quantiles years = do
  let year0 = fe0 ^. feCurrentDate
      nw0 = netWorth cs0 fe0
      sorted = sortSummaries summaries
      PathSummaryAndSeed  _ medianSeed = sorted !! (length sorted `div` 2)
      csHist = cs0 & (csNeedHistory .~ True)
      getH seed = V.fromList . view (_1 . csMC . mcsHistory) <$> execOnePathPure convertLE csHist fe0 seed years
      getNW :: V.Vector DatedSummary -> V.Vector DatedMoneyValue
      getNW = V.cons (DatedMoneyValue year0 nw0) . fmap (\(DatedSummary d (FSSummary nw _ _ _ _ _))->DatedMoneyValue d nw)
      inds = qIndices (length sorted) quantiles
      quartileSeeds = view psasSeed <$> (qSubSet inds sorted)
  histories' <- sequence $ if singleThreaded
                           then map getH quartileSeeds
                           else map getH quartileSeeds `using` parList rseq
  let histories = map getNW histories'
      median0 = initialSummary cs0 fe0
  medianHist <- getH medianSeed
  return $ SimHistories histories $ V.cons median0 medianHist


nwHistFromSummaries::(VGB.Vector v1 Int,VGB.Vector v1 Double)=>[PathSummaryAndSeed]->Int->(v1 Double, v1 Int)
nwHistFromSummaries summaries bins = summariesToHistogram (sortSummaries summaries) bins


summariesToHistogram::(VGB.Vector v1 Double, VGB.Vector v1 Int)=>[PathSummaryAndSeed]->Int->(v1 Double, v1 Int)
summariesToHistogram summaries numBins =
  let nws = V.fromList $ map psasToNumber summaries
  in histogram numBins nws

initialSummary::(IsAsset a,IsFlow fl)=>CombinedState a fl le ru->FinEnv rm->DatedSummary
initialSummary cs0 fe0 =
  let nw =  netWorth cs0 fe0
      nwbo = netWorthBreakout cs0 fe0
      (inFlow,outFlow) = grossFlows (cs0 ^. (csMC.mcsCashFlows)) fe0
      in DatedSummary (fe0 ^. feCurrentDate) (FSSummary nw nwbo inFlow outFlow (MV.zero (fe0 ^. feDefaultCCY)) 0)

data DatedSummaryWithReturns = DatedSummaryWithReturns !Year !FSSummary !MoneyValue !Double

addReturns::V.Vector DatedSummary->V.Vector DatedSummaryWithReturns --[(Year,FSSummary,MoneyValue,Double)]
addReturns fs = result where
  ccy = (V.head fs) ^. dsSummary.fssNW.mCurrency
  z = MV.zero ccy
  DatedSummary dE fsE = V.last fs
  FSSummary nwF nwboF _ _ _ _ = fsE
  final = DatedSummaryWithReturns dE (FSSummary nwF nwboF z z z 0) z 0
  l = V.zip fs (V.tail fs)
  f::FSSummary->FSSummary->(MoneyValue,Double)
  f (FSSummary nw0 _ _ _ _ _) (FSSummary nw1 _ in1 out1 tax1 _) = (retM,rate) where
    nw0A = nw0 ^. mAmount
    nw1A = nw1 ^. mAmount
    inA = in1 ^. mAmount
    outA = out1 ^. mAmount
    tax1A = tax1 ^. mAmount
    returnA = (nw1A - nw0A) - (inA - outA) + tax1A
    rate = returnA / nw0A
    retM = MoneyValue returnA ccy
  g :: (DatedSummary,DatedSummary)->DatedSummaryWithReturns --(Year,FSSummary,MoneyValue,Double)
  g (DatedSummary d fs0, DatedSummary _ fs1) = DatedSummaryWithReturns d fs' retM rate where
    (retM,rate) = f fs0 fs1
    FSSummary nw0 nwbo0 _ _ _ _ = fs0
    FSSummary _ _ in1 out1 tax1 tr1 = fs1
    fs' = FSSummary nw0 nwbo0 in1 out1 tax1 tr1
  result = V.snoc (V.map g l) final -- Ick.  That snoc is O(n)


medianMode::[PathSummary]->(Maybe Year,Maybe Year)
medianMode sbs = (Just a, Just b) where
  (ZeroNW a) = sbs !! (length sbs `div` 2) -- NB: Not the median for even number. Returns earlier of central pair
  countsM = foldl' (\m (ZeroNW d)->M.insertWith (+) d (1::Int) m) M.empty sbs
  countsL = sortBy (comparing snd) $ M.toList countsM
  b = fst (last countsL)

analyzeBankruptcies::[PathSummary]->(Int,Maybe Year,Maybe Year)
analyzeBankruptcies bs = (num,median,mode) where
  sbs = sort bs
  num = length sbs
  (median,mode) = if num > 0
                  then medianMode sbs
                  else (Nothing,Nothing)

