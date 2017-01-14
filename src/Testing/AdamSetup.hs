module Testing.AdamSetup (
  initialFinEnv,
  initialCS
  ) where

import FinancialMC.Core.MoneyValue
import FinancialMC.Core.Rates
import FinancialMC.Core.FinancialStates 
import FinancialMC.Core.MCState
import FinancialMC.Core.Utilities
import FinancialMC.Core.Asset (FlowDateRange(..),TradeType(..))
import FinancialMC.Parsers.ParseTax
import qualified FinancialMC.Builders.All as SC
import FinancialMC.Builders.RateModels

import Data.Time
import Control.Monad.State.Strict
import Data.Maybe (fromJust)

{-- to play with --}

toyExchangeRates::ExchangeRateFunction
{-- This obv should involve some sort of lookup. Hacked for testing --}
toyExchangeRates USD EUR = 0.77
toyExchangeRates EUR USD = 1.0/toyExchangeRates USD EUR
toyExchangeRates _ _     = 1.0


--taxStructures = execState (loadTaxData "Tax.xml")
taxRules = fromJust $ makeTaxRules dTaxStructure MarriedFilingJointly "New York State" Nothing

--NB, this could work any number of ways.  When the Asset is built, or before it is grown, it needs to be set to a function that can evolve it.

rateModel = MultiRM [SimpleRM (Return,Stock) (normalRateModel 0.07 0.1),
                     SimpleRM (Return,Bond) (normalRateModel 0.03  0.05)] 



sfinEnv = FinEnv defaultRateTable toyExchangeRates (fromGregorian 2012 1 1) USD taxRules rateModel
setRates = changeRate (Interest,Savings) 0.03 >> 
           changeRate (Return,Stock) 0.09 >> 
           changeCurrentDate (fromGregorian 2014 1 1) 


initialFinEnv = execState setRates sfinEnv


initialFinState = zeroFinState USD

savings1 = SC.makeCashAsset "savings" "A&P" (MoneyValue 100000 USD)
savings2 = SC.makeCashAsset "savings" "Kylie" (MoneyValue 75000 USD)

bankAccount = SC.makeAccount "Citibank" "Bank" USD [savings1,savings2]

mortgage = SC.makeFixedRateMortgage "30yr Fixed 5%" 0.05 30 (MoneyValue 500000 USD) (MoneyValue 500000 USD) 
mtgeAccount = SC.makeAccount "Wells Fargo" "Mortgage" USD [mortgage]

mixedFund = SC.makeMixedFund "50/50" 0.5 (MoneyValue 100000 USD) (MoneyValue 80000 USD)
kylie529Acct = SC.makeAccount "Kylie's 529" "529" USD [mixedFund]

house = SC.makeResidentialRE "833 President St" (MoneyValue 500000 USD) (MoneyValue 450000 USD)
houseAcct = SC.makeAccount "833 President St." "Real Estate" USD [house]

fillBS = addAccount bankAccount >> 
         addAccount mtgeAccount >> 
         addAccount kylie529Acct >>
         addAccount houseAcct
balanceSheet = execState fillBS makeNewBalanceSheet


utilityExpense = SC.makeExpense "ConEd"  (MoneyValue 10000 USD) Monthly Always
salaryPayment = SC.makeSalaryPayment "Adam Salary" (MoneyValue 20833 USD) Always
kTuition = SC.makeCollegeBill "Kylie Tuition" (MoneyValue 50000 USD) "Kylie_Ed_Counter" (Between (fromGregorian 2013 1 1) (fromGregorian 2018 1 1))

fillCFD = addFlow utilityExpense >> 
          addFlow salaryPayment >> 
          addFlow kTuition
cashFlowData = execState fillCFD makeNewCashFlows

mcs0 = makeMCState balanceSheet cashFlowData initialFinEnv [] (SC.makeSweepRule "Citibank") (SC.makeTaxTradeRule "Citibank")
fillMCS =  addRule (SC.makePayFromRule "Kylie's 529" UntaxedGrowth "Kylie_Ed_Counter")
initialMCS = execState fillMCS mcs0

initialCS = CombinedState initialFinState initialMCS False