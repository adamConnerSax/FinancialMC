module Testing.SimpleSetup 
       (
         initialFinEnv,
         initialCS
       ) where

import FinancialMC.Core.MoneyValue
import FinancialMC.Core.Asset
import FinancialMC.Core.Rates
import FinancialMC.Core.FinancialStates 
import FinancialMC.Core.Utilities
import FinancialMC.Core.MCState
import FinancialMC.Core.Asset (FlowDateRange(..))
import qualified Builders.All as SC
import FinancialMC.Builders.RateModels
import FinancialMC.Parsers.ParseTax

import Data.Time
import Control.Monad.State.Strict
import Data.Maybe (fromJust)


{-- to play with --}

toyExchangeRates::ExchangeRateFunction
{-- This obv should involve some sort of lookup. Hacked for testing --}
toyExchangeRates USD EUR = 0.8
toyExchangeRates EUR USD = 1.0/toyExchangeRates USD EUR
toyExchangeRates _ _     = 1.0

taxRules = fromJust $ makeTaxRules dTaxStructure MarriedFilingJointly "New York State" (Just "New York City")

--NB, this could work any number of ways.  When the Asset is built, or before it is grown, it needs to be set to a function that can evolve it.

rateModel = MultiRM [SimpleRM (Return,Stock) (normalRateModel 0.07 0.1),
                     SimpleRM (Return,Bond) (normalRateModel 0.03  0.05)] 
                
                
sfinEnv = FinEnv defaultRateTable  toyExchangeRates (fromGregorian 2012 1 1) USD taxRules rateModel

setRates = changeRate (Interest,Savings) 0.03 >> 
           changeRate (Return,Stock) 0.09 >> 
           changeCurrentDate (fromGregorian 2014 1 1) 

initialFinEnv = execState setRates sfinEnv

initialFinState = zeroFinState USD

savings1 = SC.makeCashAsset "Savings" "A&P" (MoneyValue 100000 USD)
bankAccount = SC.makeAccount "CitiBank" "Bank" USD [savings1]

mixedFund = SC.makeMixedFund "Stock" 1 (MoneyValue 100000 USD) (MoneyValue 80000 USD)
fundAccount = SC.makeAccount "Vanguard" "Brokerage" USD [mixedFund]

fillBS = addAccount bankAccount >> 
         addAccount fundAccount
         
balanceSheet = execState fillBS makeNewBalanceSheet


utilityExpense = SC.makeExpense "Utilities" (MoneyValue 100 USD) Monthly Always
cashFlowData = execState (addFlow utilityExpense) makeNewCashFlows 

mcs0 = makeMCState balanceSheet cashFlowData initialFinEnv [] (SC.makeSweepRule "CitiBank") (SC.makeTaxTradeRule "CitiBank")
initialCS = CombinedState initialFinState mcs0 False