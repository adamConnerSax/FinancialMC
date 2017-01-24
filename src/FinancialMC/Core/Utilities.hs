{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module FinancialMC.Core.Utilities 
       (
         readOnly
       , multS
       , multR
       , note
       , FMCException(..)
       , eitherToIO
       , noteM
       , mapSl
       , mapMSl
       , mapSr
       , buildStringSl
       , TimeInterval
       , Frequency(..)
       , frequencyMultiplier
       , Year
       , DateRange(..)
       , between
       ) where

import Control.Error (note)
import Control.Monad.Reader (ReaderT(ReaderT),runReaderT)
import Control.Monad.State.Strict (StateT(StateT),liftM)
import Control.Monad.Catch (Exception,SomeException,throwM,MonadThrow)
import Data.Typeable (Typeable)
import qualified Data.Foldable as F

import Data.Aeson (ToJSON,FromJSON)
import GHC.Generics (Generic)

readOnly :: Monad m => ReaderT s m r -> StateT s m r
readOnly readerT = StateT  (\x -> do
  r <- runReaderT readerT x
  return (r, x))


multS::Monad m => StateT a (StateT b m) r -> StateT (a,b) m r
multS (StateT k) = StateT (\(x,y) -> do 
                       let (StateT l) = k x
                       ((r,newX),newY) <- l y
                       return (r,(newX,newY)))

multR::Monad m => ReaderT a (ReaderT b m) r -> ReaderT (a,b) m r
multR (ReaderT k) = ReaderT (\(x,y) -> do 
                       let (ReaderT l) = k x
                       l y)

type Err = String 
data FMCException = FailedLookup String | BadParse String | Other String deriving (Show,Typeable)
instance Exception FMCException

eitherToIO::Either SomeException a->IO a
eitherToIO x = case x of
  Left e -> throwM e
  Right a -> return a


noteM::MonadThrow m=>FMCException->Maybe a->m a
noteM exc ma = 
  let res = note exc ma
  in case res of
    Left exception -> throwM exception
    Right x -> return x

--functions to fold over foldable while accumulating a list of results and using another piece of state (s0).
mapSl::F.Foldable t=>(a->s->(b,s))->s->t a->[b] 
mapSl f s0 l = fst $ F.foldl (\(bs,s) a->g bs a s) ([],s0) l where
  g x y z = (x ++ [fst q], snd q) where
    q = f y z

mapMSl::(F.Foldable t, Monad m)=>(a->s->m (b,s))->s->t a->m [b] 
mapMSl f s0 l = do
  let g::Monad m=>(a->s->m (b,s))->m ([b],s)->a->m ([b],s)
      g h mxs a  = do 
        (bs,s) <- mxs 
        (b,s') <- h a s 
        return  (bs++[b],s')
  liftM fst $ F.foldl (g f) (return ([],s0)) l 
  
mapSr::F.Foldable t=>(a->s->(b,s))->s->t a->[b] 
mapSr f s0 l = fst $ F.foldr (\a (bs,s)->g bs a s) ([],s0) l where
  g x y z = (x ++ [fst q], snd q) where
    q = f y z


buildStringSl::F.Foldable t => (a->String->(String,String))->String->t a->String 
buildStringSl f s0 l = mconcat $ mapSl f s0 l


type TimeInterval = Double --years

data Frequency = Annually | Monthly | Weekly | Daily deriving (Show,Read,Generic,FromJSON,ToJSON)
frequencyMultiplier::Frequency->Int
frequencyMultiplier Annually = 1
frequencyMultiplier Monthly = 12
frequencyMultiplier Weekly  = 52
frequencyMultiplier Daily = 365


type Year = Int

data DateRange = Never | Always | Starting Year | Ending Year | Only Year | Between Year Year deriving (Show,Read,Generic,FromJSON,ToJSON)

between::Year->DateRange->Bool
between _ Always = True
between day (Starting s) = s <= day 
between day (Ending e) = day <= e
between day (Only e) = day == e
between day (Between s e) = (s <= day) && (day <= e)  
between _ Never = False

{-
data FMCComponentConverters ab a leb le  =
  FMCComponentConverters
  {
    assetF::(ab->a),
    lifeEventF::(leb->le)
  }

class FMCConvertible f where
  fmcMap::FMCComponentConverters ab a leb le->f ab leb->f a le

-}
--class TypeNamed a where
--  typeName::a->String
