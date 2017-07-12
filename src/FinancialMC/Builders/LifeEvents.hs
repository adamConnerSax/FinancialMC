{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module FinancialMC.Builders.LifeEvents (
    BaseLifeEvent(..)
  , BaseLifeEventDetails(..)
  , PropertyPurchase(..)
  ) where

import           FinancialMC.Core.MoneyValue (MoneyValue,HasMoneyValue(..),Currency)
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.CValued ((|+|),(|-|))
import           FinancialMC.Core.Utilities (DateRange(..),Frequency(..))
import           FinancialMC.Core.Result (appendAndReturn)
import           FinancialMC.Core.Asset (AssetCore(AssetCore),Account(Account),AccountGetter)
import           FinancialMC.Core.Flow  (FlowCore(FlowCore))
import           FinancialMC.Core.FinancialStates (HasFinEnv(feExchange),FinEnv, ReadsFinEnv (getFinEnv), ReadsFinState (getFinState))
import           FinancialMC.Core.LifeEvent (LifeEventCore(..),IsLifeEvent(..),LifeEventConverters(LEC),
                                             LifeEventAppC,LifeEventOutput(LifeEventOutput))

import           FinancialMC.Core.TradingTypes (AccountType(PrimaryHome))

import           FinancialMC.Builders.Assets (BaseAsset(..), FixedRateMortgageDetails(..), BaseAssetDetails(ResidentialRE,FixedRateMortgage))
import           FinancialMC.Builders.Flows (BaseFlow(..),BaseFlowDetails(Expense,DeductibleExpense))

import           Control.Lens (magnify,(^.))
import           Control.Monad.Trans.Class (MonadTrans,lift)
import           Control.Monad.State (MonadState)

import Data.Aeson (genericToJSON, genericParseJSON)
import           Data.Aeson.Types (Options(fieldLabelModifier),defaultOptions,FromJSON(..),ToJSON(..))
import qualified Data.Text as T
import           GHC.Generics (Generic)

-- TODO: Lensify
data BaseLifeEvent = BaseLifeEvent { leCore::LifeEventCore,  leDetails::BaseLifeEventDetails } deriving (Generic)

instance ToJSON BaseLifeEvent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON BaseLifeEvent where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

-- Here we can add new constructors for new types of event (Retirement?  Child?)
data BaseLifeEventDetails = BuyProperty PropertyPurchase deriving (Generic)
instance ToJSON BaseLifeEventDetails
instance FromJSON BaseLifeEventDetails

-- purchase real estate and take out a mortgage.  
-- Creates an account to house the real estate asset as well as the mortgage.

-- I split this out so I could keep the field names without putting them in a sum type
data PropertyPurchase = PropertyPurchase { ppPropertyName:: !T.Text,
                                           ppPropertyValue:: !MoneyValue,
                                           ppDownPayment:: !MoneyValue,
                                           ppCostsInCash:: !MoneyValue,
                                           ppCostsInMortgage:: !MoneyValue,
                                           ppMortgageRate:: !Double,
                                           ppMortgageTerm:: !Int,
                                           ppAnnualInsurance:: !MoneyValue,
                                           ppAnnualTax:: !MoneyValue,
                                           ppAnnualMaintenance:: !MoneyValue
                                         } deriving (Generic)

instance ToJSON PropertyPurchase where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON PropertyPurchase where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance IsLifeEvent BaseLifeEvent where
  type AssetType BaseLifeEvent = BaseAsset
  type FlowType BaseLifeEvent = BaseFlow
  lifeEventCore (BaseLifeEvent lec _) = lec
  doLifeEvent = doBaseLifeEvent

instance Show BaseLifeEvent where
  show (BaseLifeEvent lec (BuyProperty pp)) = printPropertyPurchase lec pp

doBaseLifeEvent :: LifeEventAppC s a fl rm m => BaseLifeEvent -> LifeEventConverters a fl BaseLifeEvent -> AccountGetter m a -> m ()
-- should this get fixed to run underneath ResultT until the end?
doBaseLifeEvent (BaseLifeEvent (LifeEventCore name y)
                 (BuyProperty (PropertyPurchase pName pValue downPmt cashC finC rate term ins tax maint)))
                 (LEC convertA convertF) _ = do
  let propertyA = convertA $ BaseAsset (AssetCore pName pValue pValue) ResidentialRE
      ccy = pValue ^. mCurrency
      borrowedF val dp c = CV.cvNegate (val |-| dp |+| c)
  borrowed <- CV.asERMV ccy $ borrowedF (CV.fromMoneyValue pValue) (CV.fromMoneyValue downPmt) (CV.fromMoneyValue finC)     
  let mortgageA = convertA $ BaseAsset (AssetCore (T.append pName (T.pack "_mortgage")) borrowed borrowed) (FixedRateMortgage (FixedRateMortgageDetails rate term)) 
      pAccount = Account name PrimaryHome ccy [propertyA,mortgageA]
  cashP <- CV.asERMV ccy $ CV.fromMoneyValue downPmt |+| CV.fromMoneyValue cashC 
  let cashExpense = convertF $ BaseFlow (FlowCore (T.append pName (T.pack " down payment and costs.")) cashP Annually (Only y)) Expense
      insExpense = convertF $ BaseFlow (FlowCore (T.append pName (T.pack " insurance.")) ins Annually (Starting y)) Expense
      taxExpense = convertF $ BaseFlow (FlowCore (T.append pName (T.pack " property tax")) tax Annually (Starting y)) DeductibleExpense
      maintExpense = convertF $ BaseFlow (FlowCore (T.append pName (T.pack " maintenance.")) maint Annually (Starting y)) Expense
  appendAndReturn (LifeEventOutput [pAccount] [cashExpense,insExpense,taxExpense,maintExpense]) ()


    
printPropertyPurchase::LifeEventCore->PropertyPurchase->String
printPropertyPurchase (LifeEventCore n y) (PropertyPurchase pn pv dp cic cim mr mt ins tax maint) =
  "BuyProperty (" ++ show n ++ "): In " ++ show y ++ ", buy " 
  ++ show pn ++ " for " ++ show pv ++ ", putting " 
  ++ show dp ++ " down and financing the remainder with a " 
  ++ show (100*mr) ++ "%/" ++ show mt ++ " year mortgage. " ++ show cic
  ++ " in costs will be paid in cash and " ++ show cim 
  ++ " in costs will be rolled into mortgage."
  ++ " Taxes=" ++ show tax ++ "/yr; "
  ++ "insurance=" ++ show ins ++ "/yr; " 
  ++ "maintenance=" ++ show maint ++ "/yr"
                                                 

  



