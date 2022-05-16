{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module FinancialMC.Builders.Flows
  (
    BaseFlow(..)
  , HasBaseFlow (..)
  , BaseFlowDetails(..)
  ) where

import           FinancialMC.Core.Asset           (AccountName)
import           FinancialMC.Core.CValued         (cvMax, (|-|))
import qualified FinancialMC.Core.CValued         as CV
import           FinancialMC.Core.Evolve          (AccumResult (..),
                                                   Evolvable (evolve),
                                                   EvolveOutput (..), Evolver,
                                                   FlowResult (..),
                                                   TaxAmount (..))
import           FinancialMC.Core.FinancialStates (AccumName, FinEnv,
                                                   HasFinEnv (..),
                                                   ReadsFinEnv (getFinEnv))
import           FinancialMC.Core.Flow            (FlowCore, FlowDirection (..),
                                                   IsFlow (..),
                                                   annualFlowAmount, flowAmount,
                                                   flowCurrency, flowingAt,
                                                   revalueFlowCore)
import           FinancialMC.Core.MoneyValue      (Currency, MoneyValue)
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Rates           (InflationType (..),
                                                   RateTag (..), rateRequest)
import           FinancialMC.Core.Result          (MonadResult (..), ResultT)
import           FinancialMC.Core.Tax             (TaxType (..))
import           FinancialMC.Core.Utilities       (Year)

import           Control.Exception                (SomeException)
import           Control.Lens                     (makeClassy, use)

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Aeson.Types                 (Options (fieldLabelModifier),
                                                   defaultOptions)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)


flowF :: IsFlow f => Year -> f -> MoneyValue
flowF date flow = if flowingAt date flow then annualFlowAmount flow else MV.zero (flowCurrency flow)

data BaseFlowDetails =
  Expense |
  DeductibleExpense |
  StateAndLocalTax |
  EducationExpense !AccountName |
  HealthCareExpense !Bool | -- is it deductible?
  Payment !Double | -- growth rate
  SalaryPayment |
  RentalIncome !MoneyValue {- max annual deduction -} deriving (Generic,ToJSON,FromJSON)

baseFlowDirection :: BaseFlowDetails -> FlowDirection
baseFlowDirection Expense               = OutFlow
baseFlowDirection DeductibleExpense     = OutFlow
baseFlowDirection StateAndLocalTax      = OutFlow
baseFlowDirection (EducationExpense _)  = OutFlow
baseFlowDirection (HealthCareExpense _) = OutFlow
baseFlowDirection (Payment _)           = InFlow
baseFlowDirection SalaryPayment         = InFlow
baseFlowDirection (RentalIncome _)      = InFlow

instance Show BaseFlowDetails where
  show fl@Expense = "Regular Expense [" ++ show (baseFlowDirection fl) ++ "]->"
  show fl@DeductibleExpense = "Deductible Expense-> [" ++ show (baseFlowDirection fl) ++ "]->"
  show fl@StateAndLocalTax = "State and Local Tax-> [" ++ show (baseFlowDirection fl) ++ "]->"
  show fl@(EducationExpense an) = "Educational expense [" ++ show (baseFlowDirection fl) ++ ", (paid from " ++ show an ++ ")]->"
  show fl@(HealthCareExpense d) = "Healthcare expense [" ++ show (baseFlowDirection fl) ++ "; deductible=" ++ show d ++  "]->"
  show fl@(Payment gr) = "Payment [" ++ show (baseFlowDirection fl) ++ "; grows at " ++ show (100*gr) ++ "%]->"
  show fl@SalaryPayment = "Salary [" ++ show (baseFlowDirection fl) ++ "]->"
  show fl@(RentalIncome md) = "Rental income [" ++ show (baseFlowDirection fl) ++ "; max annual deduction=" ++ show md ++ "]->"

data BaseFlow = BaseFlow { _bfCore :: FlowCore, _bfDetails :: BaseFlowDetails} deriving (Generic,ToJSON,FromJSON)
makeClassy ''BaseFlow

instance Show BaseFlow where
  show (BaseFlow fc fdet) = show fdet ++ show fc

instance IsFlow BaseFlow where
  flowCore (BaseFlow fc _) = fc
  revalueFlow (BaseFlow fc fdet) v' = BaseFlow (revalueFlowCore fc v') fdet
  flowDirection (BaseFlow _ det) = baseFlowDirection det

instance Evolvable BaseFlow where
  evolve fl@(BaseFlow _ Expense) = expenseWithInflation Nothing T.empty Price fl
  evolve fl@(BaseFlow _ DeductibleExpense) = expenseWithInflation (Just OrdinaryIncome) T.empty Price fl -- not sure Price inflation is right here...
  evolve fl@(BaseFlow _ StateAndLocalTax) = expenseWithInflation (Just StateAndLocal) T.empty Price fl
  evolve fl@(BaseFlow _ (EducationExpense fr)) = expenseWithInflation Nothing fr Education fl
  evolve fl@(BaseFlow _ (HealthCareExpense d)) = let mTt = if d then (Just OrdinaryIncome) else Nothing in expenseWithInflation mTt T.empty HealthCare fl
  evolve fl@(BaseFlow _ (Payment _)) = paymentEvolve fl
  evolve fl@(BaseFlow _ SalaryPayment) = wageEvolve fl
  evolve fl@(BaseFlow _ (RentalIncome _)) = rentalIncomeEvolve fl
  {-# INLINE evolve #-}

expenseWithInflation :: IsFlow f => Maybe TaxType -> AccumName -> InflationType -> Evolver s rm m f
expenseWithInflation mDeductible accumName iType f = do
  infRate <- rateRequest (Inflation iType)
  curDate <- use $ getFinEnv.feCurrentDate
  let newA = MV.multiply (flowAmount f) (1.0 + infRate)
      expense = flowF curDate f
      cashFlow = MV.negate expense
      newExpense = revalueFlow f newA
      flowResult = case mDeductible of
        (Just tt) AllDeductible (TaxAmount tt expense)
        Nothing -> else UnTaxed cashFlow
      accums = if T.null accumName then [] else [AddTo accumName cashFlow]
  appendAndReturn (EvolveOutput [flowResult] accums) newExpense
{-# INLINE expenseWithInflation #-}

paymentEvolve :: Evolver s rm m BaseFlow
paymentEvolve p@(BaseFlow _ (Payment growth_rate)) = do
  curDate <- use $ getFinEnv.feCurrentDate
  -- only grows once live.  So a future starting payment starts at amount given
  let newA = if flowingAt curDate p then MV.multiply (flowAmount p) (1.0 + growth_rate) else flowAmount p
      cashFlow = flowF curDate p
  appendAndReturn (EvolveOutput [AllTaxed (TaxAmount OrdinaryIncome cashFlow)] []) (revalueFlow p newA)
{-# INLINE paymentEvolve #-}

wageEvolve :: Evolver s rm m BaseFlow
wageEvolve p = do
  (cashFlow',newA') <- do
    infRate <- rateRequest (Inflation Wage)
    curDate <- use $ getFinEnv.feCurrentDate
    let newA = MV.multiply (flowAmount p) (1.0 + infRate)
        cashFlow = flowF curDate p
    return (cashFlow,newA)
  appendAndReturn (EvolveOutput [AllTaxed (TaxAmount OrdinaryIncome cashFlow')] []) (revalueFlow p newA')
{-# INLINE wageEvolve #-}

--Need test for this.
rentalIncomeEvolve :: Evolver s rm m BaseFlow
rentalIncomeEvolve ri@(BaseFlow _ (RentalIncome maxAnnualDed)) = do
  let ccy = flowCurrency ri
  (cashFlow',newPayment',taxable') <- do
    infRate <- rateRequest (Inflation Price)
    curDate <- use $ getFinEnv.feCurrentDate
    let newA = MV.multiply (flowAmount ri) (1.0 + infRate)
        newPayment = revalueFlow ri newA
        cashFlow = flowF curDate ri
    taxable <- CV.asERMV ccy $ cvMax (CV.mvZero ccy) (CV.fromMoneyValue cashFlow |-| CV.fromMoneyValue maxAnnualDed)
    return (cashFlow,newPayment,taxable)
  appendAndReturn (EvolveOutput [PartiallyTaxed cashFlow' (TaxAmount NonPayrollIncome taxable')] []) newPayment'
{-# INLINE rentalIncomeEvolve #-}

{-
data Expense = Expense { eFlowCore:: !FlowCore } deriving (Generic)

instance Show Expense where
  show e = "Regular Expense [" ++ show (flowDirection e) ++ "]->" ++ show (flowCore e)

instance Evolvable Expense where
  evolve = expenseWithInflation False T.empty Price

instance TypeNamed Expense

instance IsFlow Expense where
  flowCore = eFlowCore
  revalueFlow e v' = Expense (revalueFlowCore (flowCore e) v')
  flowDirection = const OutFlow

$(deriveJSON defaultOptions{fieldLabelModifier= drop 1} ''Expense)

data DeductibleExpense = DeductibleExpense { deFlowCore:: !FlowCore } deriving (Generic)

instance Show DeductibleExpense where
  show de =  "Deductible Expense-> [" ++ show (flowDirection de) ++ "] " ++ show (flowCore de)

instance Evolvable DeductibleExpense where
  evolve = expenseWithInflation True T.empty Price

instance TypeNamed DeductibleExpense

instance IsFlow DeductibleExpense where
  flowCore = deFlowCore
  revalueFlow de v' = DeductibleExpense (revalueFlowCore (flowCore de) v')
  flowDirection = const OutFlow

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''DeductibleExpense)

data EducationalExpense = EducationalExpense {eeFlowCore:: !FlowCore, eePaidFrom:: !AccountName} deriving (Generic)

instance Show EducationalExpense where
  show f@(EducationalExpense core fr) = "Educational expense [" ++ show (flowDirection f) ++ ", (paid from " ++ show fr ++ ")]->" ++ show core

instance Evolvable EducationalExpense where
  evolve e@(EducationalExpense _ fr) = expenseWithInflation False fr Education e

instance TypeNamed EducationalExpense

instance IsFlow EducationalExpense where
  flowCore = eeFlowCore
  revalueFlow (EducationalExpense c f) v' = EducationalExpense (revalueFlowCore c v') f
  flowDirection = const OutFlow


$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''EducationalExpense)

data HealthCareExpense = HealthCareExpense {heFlowCore:: !FlowCore, heDeductible::Bool } deriving (Generic)

instance Show HealthCareExpense where
  show f@(HealthCareExpense core d) = "Healthcare expense [" ++ show (flowDirection f) ++ "; deductible=" ++ show d ++  "]->" ++ show core

instance Evolvable HealthCareExpense where
  evolve he@(HealthCareExpense _ d) = expenseWithInflation d T.empty HealthCare he

instance TypeNamed HealthCareExpense

instance IsFlow HealthCareExpense where
  flowCore = heFlowCore
  revalueFlow (HealthCareExpense c d) v' = HealthCareExpense (revalueFlowCore c v') d
  flowDirection = const OutFlow


$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''HealthCareExpense)

data Payment = Payment { pFlowCore:: !FlowCore, pGrowthRate:: !Double} deriving (Generic)

instance Show Payment where
  show sp@(Payment _ gr) = "Payment [" ++ show (flowDirection sp) ++ "] (grows at " ++ show (100*gr) ++ "%)->" ++ show (flowCore sp)

instance Evolvable Payment where
  evolve = paymentEvolve

instance TypeNamed Payment

instance IsFlow Payment where
  flowCore = pFlowCore
  revalueFlow (Payment c gr) v' = Payment (revalueFlowCore c v') gr
  flowDirection = const InFlow

$(deriveJSON defaultOptions{fieldLabelModifier= drop 1} ''Payment)



data SalaryPayment = SalaryPayment { spFlowCore:: !FlowCore} deriving (Generic)

instance Show SalaryPayment where
  show sp = "Salary [" ++ show (flowDirection sp) ++ "]->" ++ show (flowCore sp)

instance Evolvable SalaryPayment where
  evolve = wageEvolve

instance TypeNamed SalaryPayment

instance IsFlow SalaryPayment where
  flowCore = spFlowCore
  revalueFlow (SalaryPayment c) v' = SalaryPayment (revalueFlowCore c v')
  flowDirection = const InFlow

$(deriveJSON defaultOptions{fieldLabelModifier= drop 1} ''SalaryPayment)



data RentalIncome = RentalIncome { riFlowCore:: !FlowCore, riMaxAnnualDeduction:: !MoneyValue} deriving (Generic)

instance Show RentalIncome where
  show f@(RentalIncome c mad) = "Rental income [" ++ show (flowDirection f) ++ "]-> " ++ show c ++ "; max annual deduction=" ++ show mad

instance Evolvable RentalIncome where
  evolve = rentalIncomeEvolve

instance TypeNamed RentalIncome

instance IsFlow RentalIncome where
  flowCore = riFlowCore
  revalueFlow (RentalIncome c mad) v' = RentalIncome (revalueFlowCore c v') mad
  flowDirection = const InFlow

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''RentalIncome)

-}
