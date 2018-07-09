{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving #-}
module FinancialMC.Core.Rates
       (
         RateType(..)
       , isRateType
       , InterestType(..)
       , ReturnType(..)
       , InflationType(..)
       , RateTag(..)
       , RateTable(..)
       , HasRateTable (..)
       , ReadsRateTable (..)
       , RateUpdates
--       , fromMap
       , setToDefaults
--       , defaultMapBasedRateTable 
       , defaultArrayBasedRateTable
       , rateRequest
       , RSource
       , Rate
       , showRateAsPct
--       , RateModelC
       , IsRateModel(..)
       , runModel
       , allRateTags
       ) where


import           FinancialMC.Core.MoneyValue (Currency (..))
import           FinancialMC.Core.Utilities  (FMCException (FailedLookup),
                                              noteM)

import           Control.Lens                (Getter, Lens', use)
import           Control.Monad.State.Strict  (MonadState)
import           Data.Foldable               (foldl')
import qualified Data.Map.Lazy               as M
import           Data.Maybe                  (fromJust)
import qualified Data.Random as R
import           Data.Random.Source.PureMT   (PureMT)
import           Text.Printf                 (PrintfArg, printf)
import Data.Array ((!),(//))
import qualified Data.Array as A
import qualified Data.Ix as Ix

import           Data.Aeson                  (FromJSON (..), ToJSON (..))
import           GHC.Generics                (Generic)

import           Control.Monad.Except        (MonadError)
import qualified Data.Foldable               as F
import           Data.Monoid                 ((<>))
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as S
import qualified Data.Text                   as T

data InterestType = Savings | CreditCard deriving (Enum,Eq,Ord,Bounded,Show,Read,Generic,A.Ix,FromJSON,ToJSON)
data ReturnType = Stock | Bond | RealEstate deriving (Enum,Eq,Ord,Bounded,Show,Read,Generic,A.Ix,FromJSON,ToJSON)
data InflationType =  Price | Education | HealthCare | Wage | TaxBracket deriving (Enum,Bounded,Eq,Ord,Show,Read,Generic,A.Ix,FromJSON,ToJSON)

data RateTag = Interest !InterestType |
               Return !ReturnType |
               Inflation !InflationType |
               Exchange !Currency deriving (Ord,Eq,Read,Show,Generic,FromJSON,ToJSON)


allRateTags :: [RateTag]
allRateTags = [Interest x | x<-[(minBound::InterestType)..]] ++
              [Return x | x<-[(minBound::ReturnType)..]] ++
              [Inflation x | x<-[(minBound::InflationType)..]] ++
              [Exchange x | x<-[(minBound::Currency)..]]


-- NB: These next two must be consistent or things will not go well...
instance Enum RateTag where
  fromEnum rt =
    let nInterest = length [(minBound :: InterestType)..]
        nReturn   = length [(minBound :: ReturnType)..]
        nInflation = length [(minBound :: InflationType)..]
    in case rt of
      (Interest x) -> fromEnum x
      (Return x) -> nInterest + fromEnum x
      (Inflation x) -> nInterest + nReturn + fromEnum x
      (Exchange x) -> nInterest + nReturn + nInflation + fromEnum x

  toEnum n = allRateTags !! n

instance Bounded RateTag where
  minBound = Interest (minBound :: InterestType)
  maxBound = Exchange (maxBound :: Currency) 

--using the Int instance of Ix 
instance A.Ix RateTag where
  range (l,r) = toEnum <$> Ix.range (fromEnum l, fromEnum r)
  index (l,r) i = Ix.index (fromEnum l, fromEnum r) (fromEnum i)
  inRange (l,r) i = Ix.inRange (fromEnum l, fromEnum r) (fromEnum i)
  rangeSize (l,r) = Ix.rangeSize (fromEnum l, fromEnum r)
--  unsafeIndex (l,r) = Ix.unsafeIndex (fromEnum l, fromEnum r) (fromEnum i)


data RateType = IsInterest | IsReturn | IsInflation | IsExchange  deriving (Enum,Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

isRateType :: RateType->RateTag->Bool
isRateType IsInterest (Interest _)   = True
isRateType IsReturn (Return _)       = True
isRateType IsInflation (Inflation _) = True
isRateType IsExchange (Exchange _)   = True
isRateType _ _                       = False

type Rate = Double
type RSource = PureMT

defaultRates :: RateTag -> Rate
defaultRates (Interest _)  = 0.01
defaultRates (Return _)    = 0.05
defaultRates (Inflation _) = 0.015
defaultRates (Exchange _)  = 1.0

showRateAsPct :: Rate->String
showRateAsPct r = (printf "%.2f" (r*100)) ++ "%"

type RateUpdates a = Seq.Seq (RateTag, a)

updateRateTable :: RateUpdates a -> RateTable a -> RateTable a
updateRateTable updates curRates = rBulkUpdate curRates updates 


data RateTable a = RateTable { rLookup :: RateTag -> a
                             , rSet :: RateTag -> a -> RateTable a
                             , rBulkUpdate :: RateUpdates a -> RateTable a
                             , rToList :: [(RateTag, a)]
                             , rKeys:: [RateTag]
                             }

class HasRateTable s a | s -> a where
  rateTable :: Lens' s (RateTable a)

class ReadsRateTable s a | s -> a where
  getRateTable :: Getter s (RateTable a)
  default getRateTable :: HasRateTable s a => Getter s (RateTable a)
  getRateTable = rateTable

instance (PrintfArg a, Num a) => Show (RateTable a) where
  show rt = "[" ++ show (fmap f (rKeys rt)) ++ "]" where
    fmtRate x = printf "%.2f" (x*100)
    f k = "(" ++ show k ++ "," ++ fmtRate (rLookup rt k) ++ "%)"

{-
-- This used the different interface when lookup returned a (Maybe a)
fromMap :: M.Map RateTag a -> RateTable a
fromMap m = RateTable (`M.lookup` m) (\t r->fromMap $ M.insert t r m) (fromMap . flip M.union m . M.fromList . F.toList) (M.toList m) (M.keys m)


defaultMapBasedRateTable :: RateTable Double
defaultMapBasedRateTable = setToDefaults $ fromMap M.empty
-}

fromArray :: A.Array RateTag a -> RateTable a
fromArray rates = RateTable
  (\t -> rates ! t)
  (\t r -> fromArray $ rates // [(t,r)])
  (\upds -> fromArray $ rates // (F.toList upds))
  (A.assocs rates)
  (A.indices rates)

setToDefaults :: RateTable Rate -> RateTable Rate
setToDefaults t = foldl' (\tbl key-> (rSet tbl) key (defaultRates key)) t allRateTags

defaultArrayBasedRateTable :: RateTable Double
defaultArrayBasedRateTable = setToDefaults $ fromArray (A.listArray (minBound :: RateTag, maxBound :: RateTag) (repeat 0.0))

{-
throwingLookup :: MonadError FMCException m => RateTable a -> RateTag -> m a
throwingLookup rt t = noteM (FailedLookup ((T.pack $ show t) <> ": rate not found")) $ rLookup rt t
-}

rateRequest :: ({- MonadError FMCException m,-} MonadState s m, ReadsRateTable s a) => RateTag -> m a
rateRequest rTag = use getRateTable >>= return . flip rLookup rTag {- flip throwingLookup rTag -}

applyRateUpdates :: IsRateModel r  => RateTable Rate -> r -> R.RVar (r, RateTable Rate)
applyRateUpdates curRates model = do
  (newModel, updates) <- rateModelF curRates model
  return $ (newModel, rBulkUpdate curRates updates)

runModel :: IsRateModel r  => RateTable Rate -> r -> RSource -> ((r, RateTable Rate), RSource)
runModel curRates model = R.sampleState (applyRateUpdates curRates model)

-- models will absorb sub-models.  So if you model stock returns as a function of bond returns, you will make a model for both.  And if a third thing depends on either you will include it as well.  So that the final set of models will be non-overlapping in affected rates.
-- models which include other models can verify this at runtime, using the updatedRates typeclass function.
-- can I force verification? Even better do it with types? How? Where?

class IsRateModel r where
  updatedRates :: r -> S.Set RateTag
  rateModelF :: RateTable Rate -> r -> R.RVar (r, RateUpdates Rate)

