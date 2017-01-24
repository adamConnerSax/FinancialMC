{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module FinancialMC.Core.Flow
       (
         flowingAt,
         FlowName,
         FlowCore(..),
         FlowDirection(..),
         revalueFlowCore,
         flowName,
         flowAmount,
         flowFrequency,
         flowDateRange,
         annualFlowAmount,
         flowCurrency,
         IsFlow(..),
--         Flow(..),
         between
       ) where


import           FinancialMC.Core.MoneyValue (MoneyValue,mCurrency,Currency(..))
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Evolve (Evolvable(..))
--import           FinancialMC.Core.Result ()
import           FinancialMC.Core.Utilities (Frequency,DateRange,between,frequencyMultiplier)

import           Control.Lens ((^.))
import           Control.Monad (liftM)
import  GHC.Generics (Generic)
import           Data.Aeson (ToJSON(..))
import Data.Aeson (ToJSON(..),FromJSON(..),genericToJSON,genericParseJSON)
import           Data.Aeson.Types (Options(fieldLabelModifier),defaultOptions)
import qualified Data.Text as T

type FlowName = T.Text

data FlowCore = FlowCore {fcName:: !FlowName,
                          fcAmount:: !MoneyValue,
                          fcFrequency:: !Frequency,
                          fcDateRange:: !DateRange } deriving (Generic)
                
instance Show FlowCore where
   show (FlowCore n a ff dr) = show n ++ " (" ++ show a ++ ", " ++ show ff ++ " " ++ show dr ++ ")"

instance ToJSON FlowCore where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier= drop 2}
instance FromJSON FlowCore where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier= drop 2}
  
{- $(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''FlowCore) -}

revalueFlowCore::FlowCore->MoneyValue->FlowCore
revalueFlowCore (FlowCore n _ f dr) v' = FlowCore n v' f dr

data FlowDirection = InFlow | OutFlow deriving(Eq,Show)

class Evolvable f=>IsFlow f where
  flowCore::f->FlowCore
  revalueFlow::f->MoneyValue->f
  flowDirection::f->FlowDirection  
  
flowName::IsFlow f=>f->FlowName
flowName f = fcName $ flowCore f

flowAmount::IsFlow f=>f->MoneyValue
flowAmount f = fcAmount $ flowCore f

flowCurrency::IsFlow f=>f->Currency
flowCurrency f = (flowAmount f) ^. mCurrency

flowFrequency::IsFlow f=>f->Frequency
flowFrequency f = fcFrequency $ flowCore f

flowDateRange::IsFlow f=>f->DateRange
flowDateRange f = fcDateRange $ flowCore f

flowingAt::IsFlow f=>Int->f->Bool
flowingAt day f = between day (flowDateRange f) 

annualFlowAmount::IsFlow f=>f->MoneyValue
annualFlowAmount f = MV.multiply (flowAmount f) (fromIntegral $ frequencyMultiplier (flowFrequency f))

{-
data Flow where
  MkFlow::(IsFlow f, Show f, ToJSON f)=>f->Flow

instance Show Flow where
  show (MkFlow f) = show f
  
instance Evolvable Flow where
  evolve (MkFlow f) = liftM MkFlow (evolve f)

instance TypeNamed Flow where
  typeName (MkFlow a) = typeName a


instance IsFlow Flow where
  flowCore (MkFlow f) = flowCore f
  revalueFlow (MkFlow f) x = MkFlow (revalueFlow f x) 
  flowDirection (MkFlow f) = flowDirection f

instance JSON_Existential Flow where
  containedName (MkFlow a) = typeName a
  containedJSON (MkFlow a) = toJSON a

instance ToJSON Flow where
  toJSON = existentialToJSON 

instance HasParsers e Flow => EnvFromJSON e Flow where
  envParseJSON = parseJSON_Existential
-}
