{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           FinancialMC.Core.Flow  (Flow(MkFlow),FlowCore(FlowCore))
import           FinancialMC.Core.FinancialStates (HasFinEnv(feExchange),FinEnv)
import           FinancialMC.Core.LifeEvent (LifeEventCore(..),IsLifeEvent(..),LifeEventApp,LifeEventOutput(LifeEventOutput))

import           FinancialMC.Core.TradingTypes (AccountType(PrimaryHome))

import           FinancialMC.Builders.Assets (FMCBaseAsset(..),FMCBaseAssetDetails(ResidentialRE,FixedRateMortgage))
import           FinancialMC.Builders.Flows (Expense(Expense),DeductibleExpense(DeductibleExpense))

import           Control.Lens (magnify,(^.))
import           Control.Monad.Trans.Class (MonadTrans,lift)
import           Control.Monad.Reader (ReaderT)

import Data.Aeson (genericToJSON, genericParseJSON)
import           Data.Aeson.Types (Options(fieldLabelModifier),defaultOptions,FromJSON(..),ToJSON(..))
import Data.Aeson.Existential (EnvFromJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)

--laERMV::_
laERMV::(MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 n), n ~ ReaderT FinEnv m)=>Currency->CV.CVD->t1 (t2 (ReaderT FinEnv m)) MoneyValue
laERMV c a = lift . lift . magnify feExchange $ CV.asERMV c a



data BaseLifeEvent = BaseLifeEvent { leCore::LifeEventCore,  leDetails::BaseLifeEventDetails } deriving (Generic)
instance ToJSON BaseLifeEvent where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON BaseLifeEvent where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance EnvFromJSON e BaseLifeEvent

-- Here we can add new constructors for new types of event (Retirement?  Child?)
data BaseLifeEventDetails = BuyProperty PropertyPurchase deriving (Generic)
instance ToJSON BaseLifeEventDetails
instance FromJSON BaseLifeEventDetails
instance EnvFromJSON e BaseLifeEventDetails

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
instance EnvFromJSON e PropertyPurchase



instance IsLifeEvent BaseLifeEvent where
  type AssetType BaseLifeEvent = FMCBaseAsset
  lifeEventCore (BaseLifeEvent lec _) = lec
  doLifeEvent ble@(BaseLifeEvent lec (BuyProperty pp)) = buyProperty lec pp


instance Show BaseLifeEvent where
  show (BaseLifeEvent lec (BuyProperty pp)) = printPropertyPurchase lec pp


-- should this get fixed to run underneath ResultT until the end?
buyProperty::LifeEventCore ->PropertyPurchase -> (FMCBaseAsset -> a) ->AccountGetter a->LifeEventApp a ()
buyProperty (LifeEventCore name y) (PropertyPurchase pName pValue downPmt cashC finC rate term ins tax maint) convert _ = do
  let propertyA = convert $ FMCBaseAsset (AssetCore pName pValue pValue) ResidentialRE
      ccy = pValue ^. mCurrency
      borrowedF val dp c = CV.cvNegate (val |-| dp |+| c)
  borrowed <- laERMV ccy $ borrowedF (CV.fromMoneyValue pValue) (CV.fromMoneyValue downPmt) (CV.fromMoneyValue finC)     
  let mortgageA = convert $ FMCBaseAsset (AssetCore (T.append pName (T.pack "_mortgage")) borrowed borrowed) (FixedRateMortgage rate term) 
      pAccount = Account name PrimaryHome ccy [propertyA,mortgageA]
  cashP <- laERMV ccy $ CV.fromMoneyValue downPmt |+| CV.fromMoneyValue cashC 
  let cashExpense = MkFlow $ Expense (FlowCore (T.append pName (T.pack " down payment and costs.")) cashP Annually (Only y))
      insExpense = MkFlow $ Expense (FlowCore (T.append pName (T.pack " insurance.")) ins Annually (Starting y))
      taxExpense = MkFlow $ DeductibleExpense (FlowCore (T.append pName (T.pack " property tax")) tax Annually (Starting y))
      maintExpense = MkFlow $ Expense (FlowCore (T.append pName (T.pack " maintenance.")) maint Annually (Starting y))
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
                                                 

  



