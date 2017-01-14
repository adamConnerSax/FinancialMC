{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell, FlexibleContexts, TypeFamilies #-}
module FinancialMC.Builders.LifeEvents (
  BuyProperty(..)
  ) where

import           FinancialMC.Core.MoneyValue (MoneyValue,HasMoneyValue(..),Currency)
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.CValued ((|+|),(|-|))
import           FinancialMC.Core.Utilities (DateRange(..),Frequency(..),Year)
import           FinancialMC.Core.Result (appendAndReturn)
import           FinancialMC.Core.Asset (Asset(MkAsset),AssetCore(AssetCore),Account(Account),AccountGetter)
import           FinancialMC.Core.Flow  (Flow(MkFlow),FlowCore(FlowCore))
import           FinancialMC.Core.FinancialStates (HasFinEnv(feExchange),FinEnv)
import           FinancialMC.Core.LifeEvent (LifeEventName,IsLifeEvent(..),LifeEventApp,LifeEventOutput(LifeEventOutput))

import           FinancialMC.Core.TradingTypes (AccountType(PrimaryHome))

import           FinancialMC.Builders.Assets (ResidentialRE(ResidentialRE),FixedRateMortgage(FixedRateMortgage))
import           FinancialMC.Builders.Flows (Expense(Expense),DeductibleExpense(DeductibleExpense))

import           Control.Lens (magnify,(^.))
import           Control.Monad.Trans.Class (MonadTrans,lift)
import           Control.Monad.Reader (ReaderT)

import           Data.Aeson.Types (Options(fieldLabelModifier),defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Existential (TypeNamed)
import qualified Data.Text as T
import           GHC.Generics (Generic)

--laERMV::_
laERMV::(MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 n), n ~ ReaderT FinEnv m)=>Currency->CV.CVD->t1 (t2 (ReaderT FinEnv m)) MoneyValue
laERMV c a = lift . lift . magnify feExchange $ CV.asERMV c a

-- purchase real estate and take out a mortgage.  
-- Creates an account to house the real estate asset as well as the mortgage.

-- should this get fixed to run underneath ResultT until the end?
buyProperty::BuyProperty->AccountGetter->LifeEventApp ()
buyProperty (BuyProperty y name pName pValue downPmt cashC finC rate term ins tax maint) _ = do
  let propertyA = MkAsset $ ResidentialRE (AssetCore pName pValue pValue)
      ccy = pValue ^. mCurrency
      borrowedF val dp c = CV.cvNegate (val |-| dp |+| c)
  borrowed <- laERMV ccy $ borrowedF (CV.fromMoneyValue pValue) (CV.fromMoneyValue downPmt) (CV.fromMoneyValue finC)     
  let mortgageA = MkAsset $ FixedRateMortgage (AssetCore (T.append pName (T.pack "_mortgage")) borrowed borrowed) rate term 
      pAccount = Account name PrimaryHome ccy [propertyA,mortgageA]
  cashP <- laERMV ccy $ CV.fromMoneyValue downPmt |+| CV.fromMoneyValue cashC 
  let cashExpense = MkFlow $ Expense (FlowCore (T.append pName (T.pack " down payment and costs.")) cashP Annually (Only y))
      insExpense = MkFlow $ Expense (FlowCore (T.append pName (T.pack " insurance.")) ins Annually (Starting y))
      taxExpense = MkFlow $ DeductibleExpense (FlowCore (T.append pName (T.pack " property tax")) tax Annually (Starting y))
      maintExpense = MkFlow $ Expense (FlowCore (T.append pName (T.pack " maintenance.")) maint Annually (Starting y))
  appendAndReturn (LifeEventOutput [pAccount] [cashExpense,insExpense,taxExpense,maintExpense]) ()


data BuyProperty = BuyProperty { bpYear:: !Year, 
                                 bpName:: !LifeEventName,
                                 bpPropertyName:: !T.Text,
                                 bpPropertyValue:: !MoneyValue,
                                 bpDownPayment:: !MoneyValue,
                                 bpCostsInCash:: !MoneyValue,
                                 bpCostsInMortgage:: !MoneyValue,
                                 bpMortgageRate:: !Double,
                                 bpMortgageTerm:: !Int,
                                 bpAnnualInsurance:: !MoneyValue,
                                 bpAnnualTax:: !MoneyValue,
                                 bpAnnualMaintenance:: !MoneyValue
                               } deriving (Generic)
                
instance Show BuyProperty where
  show (BuyProperty y n pn pv dp cic cim mr mt ins tax maint) = "BuyProperty (" ++ show n ++ "): In " ++ show y ++ ", buy " 
                                                  ++ show pn ++ " for " ++ show pv ++ ", putting " 
                                                  ++ show dp ++ " down and financing the remainder with a " 
                                                  ++ show (100*mr) ++ "%/" ++ show mt ++ " year mortgage. " ++ show cic
                                                  ++ " in costs will be paid in cash and " ++ show cim 
                                                  ++ " in costs will be rolled into mortgage."
                                                  ++ " Taxes=" ++ show tax ++ "/yr; "
                                                  ++ "insurance=" ++ show ins ++ "/yr; " 
                                                  ++ "maintenance=" ++ show maint ++ "/yr"
                                                 


instance TypeNamed BuyProperty
  
instance IsLifeEvent BuyProperty where
  lifeEventName (BuyProperty _ n _ _ _ _ _ _ _ _ _ _) = n
  lifeEventYear (BuyProperty y _ _ _ _ _ _ _ _ _ _ _) = y
  doLifeEvent = buyProperty
  

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''BuyProperty)  

