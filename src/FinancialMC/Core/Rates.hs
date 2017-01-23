{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module FinancialMC.Core.Rates 
       (
         RateType(..)
       , isRateType
       , InterestType(..)
       , ReturnType(..)
       , InflationType(..)
       , RateTag(..)
       , RateTable(..)
       , fromMap
       , setToDefaults
       , defaultRateTable -- defaultRateTable is map based
       , rateRequest
       , RSource
       , Rate
       , IsRateModel(..)
       , applyModel
       , RateModelC
       ) where


import           FinancialMC.Core.Utilities (noteM,FMCException(FailedLookup))
import           FinancialMC.Core.MoneyValue (Currency(..))

import Data.Aeson.Existential (EnvFromJSON(..),JSON_Existential(..),TypeNamed(..),HasParsers,parseJSON_Existential,existentialToJSON)

import           Data.Random.Source.PureMT (PureMT)
import           Data.Maybe (fromJust)
import           Control.Monad.State.Strict (runStateT,MonadState)
import           Control.Monad.Reader (ReaderT(ReaderT),MonadReader(reader,ask),runReaderT,withReaderT)
import           Control.Monad.Catch (MonadThrow)
import qualified Data.Map.Lazy as M
import           Text.Printf (printf,PrintfArg)

import           Data.Aeson (ToJSON(..),FromJSON(..))
import           GHC.Generics (Generic)


data InterestType = Savings | CreditCard deriving (Enum,Eq,Ord,Bounded,Show,Read,Generic,FromJSON,ToJSON)
data ReturnType = Stock | Bond | RealEstate deriving (Enum,Eq,Ord,Bounded,Show,Read,Generic,FromJSON,ToJSON)
data InflationType =  Price | Education | HealthCare | Wage | TaxBracket deriving (Enum,Bounded,Eq,Ord,Show,Read,Generic,FromJSON,ToJSON)

data RateTag = Interest !InterestType | 
               Return !ReturnType | 
               Inflation !InflationType | 
               Exchange !Currency deriving (Ord,Eq,Read,Show,Generic,FromJSON,ToJSON)

instance EnvFromJSON e RateTag

allTags::[RateTag]
allTags = [Interest x | x<-[(minBound::InterestType)..]] ++ 
          [Return x | x<-[(minBound::ReturnType)..]] ++ 
          [Inflation x | x<-[(minBound::InflationType)..]] ++ 
          [Exchange x | x<-[(minBound::Currency)..]] 
          
data RateType = IsInterest | IsReturn | IsInflation | IsExchange  deriving (Enum,Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

instance EnvFromJSON e RateType

isRateType::RateType->RateTag->Bool
isRateType IsInterest (Interest _) = True
isRateType IsReturn (Return _) = True
isRateType IsInflation (Inflation _) = True
isRateType IsExchange (Exchange _) = True
isRateType _ _ = False

type Rate = Double
type RSource = PureMT

defaultRates::RateTag->Rate
defaultRates (Interest _) = 0.01
defaultRates (Return _) = 0.05
defaultRates (Inflation _) = 0.015
defaultRates (Exchange _) = 1.0

data RateTable a = RateTable { rLookup::RateTag->Maybe a, 
                               rSet::RateTag->a->RateTable a, 
                               rToList::[(RateTag,a)],
                               rKeys::[RateTag] }
                   
                   
                   
instance (PrintfArg a,Num a)=>Show (RateTable a) where
  show rt = "[" ++ show (map f (rKeys rt)) ++ "]" where
    fmtRate x = printf "%.2f" (x*100)
    f k = "(" ++ show k ++ "," ++ fmtRate (fromJust $ rLookup rt k) ++ "%)"


fromMap::M.Map RateTag a->RateTable a   
fromMap m = RateTable (`M.lookup` m) (\t r->fromMap $ M.insert t r m) (M.toList m) (M.keys m)    

setToDefaults::RateTable Double->RateTable Double
setToDefaults t = foldl (\tbl key-> (rSet tbl) key (defaultRates key)) t allTags 

defaultRateTable::RateTable Double
defaultRateTable = setToDefaults $ fromMap M.empty

throwingLookup::MonadThrow m=>RateTable a->RateTag->m a 
throwingLookup rt t = noteM (FailedLookup (show t ++ ": rate not found")) $ rLookup rt t

rateRequest::(MonadReader (RateTable a) m,MonadThrow m)=>RateTag->m a
rateRequest rTag = ask >>= flip throwingLookup rTag

type RateModelC m = (MonadState (RateTable Rate, RSource) m, MonadThrow m)

applyModel::(IsRateModel r,MonadThrow m)=>(RateTable Rate,RSource)->r->m (r,(RateTable Rate,RSource))
applyModel (rates,src) model = runStateT (rateModelF model) (rates,src)

class IsRateModel a where
  rateModelF::(MonadState (RateTable Rate, RSource) m,MonadThrow m)=>a->m a

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


