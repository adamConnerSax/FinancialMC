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
       , fromMap
       , setToDefaults
       , defaultRateTable -- defaultRateTable is map based
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
import           Control.Monad.Reader        (MonadReader (ask))
import           Control.Monad.State.Strict  (MonadState, runStateT)
import           Data.Foldable               (foldl')
import qualified Data.Map.Lazy               as M
import           Data.Maybe                  (fromJust)
import qualified Data.Random as R
import           Data.Random.Source.PureMT   (PureMT)
import           Text.Printf                 (PrintfArg, printf)

import           Data.Aeson                  (FromJSON (..), ToJSON (..))
import           GHC.Generics                (Generic)

import           Control.Monad.Except        (MonadError)
import qualified Data.Foldable               as F
import           Data.Monoid                 ((<>))
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as S
import qualified Data.Text                   as T

data InterestType = Savings | CreditCard deriving (Enum,Eq,Ord,Bounded,Show,Read,Generic,FromJSON,ToJSON)
data ReturnType = Stock | Bond | RealEstate deriving (Enum,Eq,Ord,Bounded,Show,Read,Generic,FromJSON,ToJSON)
data InflationType =  Price | Education | HealthCare | Wage | TaxBracket deriving (Enum,Bounded,Eq,Ord,Show,Read,Generic,FromJSON,ToJSON)

data RateTag = Interest !InterestType |
               Return !ReturnType |
               Inflation !InflationType |
               Exchange !Currency deriving (Ord,Eq,Read,Show,Generic,FromJSON,ToJSON)


allRateTags :: [RateTag]
allRateTags = [Interest x | x<-[(minBound::InterestType)..]] ++
              [Return x | x<-[(minBound::ReturnType)..]] ++
              [Inflation x | x<-[(minBound::InflationType)..]] ++
              [Exchange x | x<-[(minBound::Currency)..]]

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


data RateTable a = RateTable { rLookup :: RateTag -> Maybe a
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

instance (PrintfArg a, Num a)=>Show (RateTable a) where
  show rt = "[" ++ show (fmap f (rKeys rt)) ++ "]" where
    fmtRate x = printf "%.2f" (x*100)
    f k = "(" ++ show k ++ "," ++ fmtRate (fromJust $ rLookup rt k) ++ "%)"

fromMap :: M.Map RateTag a -> RateTable a
fromMap m = RateTable (`M.lookup` m) (\t r->fromMap $ M.insert t r m) (fromMap . flip M.union m . M.fromList . F.toList) (M.toList m) (M.keys m)

setToDefaults :: RateTable Rate -> RateTable Rate
setToDefaults t = foldl' (\tbl key-> (rSet tbl) key (defaultRates key)) t allRateTags

defaultRateTable :: RateTable Double
defaultRateTable = setToDefaults $ fromMap M.empty

throwingLookup :: MonadError FMCException m => RateTable a -> RateTag -> m a
throwingLookup rt t = noteM (FailedLookup ((T.pack $ show t) <> ": rate not found")) $ rLookup rt t

rateRequest :: (MonadError FMCException m, MonadState s m, ReadsRateTable s a) => RateTag -> m a
rateRequest rTag = use getRateTable >>= flip throwingLookup rTag

{-
runModel :: (IsRateModel r, R.MonadRandom m, RandomSource m RSource, MonadError FMCException m)
  => RateTable Rate -> RSource -> r -> m (r, (RateTable Rate, RSource))
runModel curRates randomSrc model = do
  (newModel, (updates, newSrc)) <- runStateT (rateModelF curRates model) (Seq.empty, randomSrc)
  return $ (newModel, (rBulkUpdate curRates updates, newSrc))
-}

runModel :: IsRateModel r  => RateTable Rate -> RSource -> r -> (r, (RateTable Rate, RSource))
runModel curRates rSource model = 
  let ((newModel, updates), newSrc) = R.sampleState (rateModelF curRates model) rSource
  in (newModel, (rBulkUpdate curRates updates, newSrc))

-- models will absorb sub-models.  So if you model stock returns as a function of bond returns, you will make a model for both.  And if a third thing depends on either you will include it as well.  So that the final set of models will be non-overlapping in affected rates.
-- models which include other models can verify this at runtime, using the updatedRates typeclass function.
-- can I force verification? Where?

--type RateModelC m = (RandomSource m RSource, MonadState (RateUpdates Rate, RSource) m)

class IsRateModel r where
  updatedRates :: r -> S.Set RateTag
  rateModelF :: RateTable Rate -> r -> R.RVar (r, RateUpdates Rate)

{-
data RateModel where
  MkRateModel::(TypeNamed a, IsRateModel a, ToJSON a) => a->RateModel

instance JSON_Existential RateModel where
  containedName (MkRateModel a) = typeName a
  containedJSON (MkRateModel a) = toJSON a

instance IsRateModel RateModel where
  rateModelF (MkRateModel x) = rateModelF x

instance ToJSON RateModel where
  toJSON = existentialToJSON

instance HasParsers e RateModel=>EnvFromJSON e RateModel where
  envParseJSON = parseJSON_Existential
-}


