{-# LANGUAGE OverloadedStrings #-}
module UnitTests (tests,mainTest) where

import FinancialMC.Base (CombinedState,HasCombinedState(..),FinEnv,HasFinEnv(..),HasMCState(..),
                        PathSummary(ZeroNW,FinalNW),getAccount,accountValue,Year,
                        execOnePathPure)

import FinancialMC.Core.Asset (AccountName)
import FinancialMC.Core.LifeEvent (IsLifeEvent(..))
import FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),Currency(USD,EUR),ExchangeRateFunction)
import FinancialMC.Core.Utilities (FMCException(..),eitherToIO)
import qualified FinancialMC.Core.MoneyValueOps as MV

import qualified FinancialMC.Parsers.Configuration as C
import FinancialMC.Parsers.ConfigurationLoader (loadConfigurations,buildInitialStateFromConfig)
import FinancialMC.Parsers.JSON.BaseTypes (baseParsers)
import FinancialMC.Base (BaseAsset,BaseLifeEvent)

import Distribution.TestSuite (run,TestInstance(TestInstance),Test(Test),Progress(Finished),Result(Pass,Fail),tags,options,name,setOption)
import Control.Lens ((^.),(&),(.~))


type TestAsset = BaseAsset
type TestLifeEvent = BaseLifeEvent

leConverter::AssetType TestLifeEvent -> TestAsset
leConverter = id

ccs::C.FMCComponentConverters BaseAsset TestAsset BaseLifeEvent TestLifeEvent
ccs = C.FMCComponentConverters id id

type FMCTestF = Int->(CombinedState TestAsset TestLifeEvent,FinEnv)->Bool
data FMCTest = FMCTest String FMCTestF
data FMCTestSet = ConfigTests String [(String,Int,[FMCTest])] |
                  StateTests  (CombinedState TestAsset TestLifeEvent,FinEnv) [(Int,[FMCTest])]


notTest::FMCTestF->FMCTestF
notTest f x y = not $ f x y 

andTest::FMCTestF->FMCTestF->Int->(CombinedState TestAsset TestLifeEvent,FinEnv)->Bool
andTest f g x y = f x y && g x y
  
andTests::[FMCTestF]->Int->(CombinedState TestAsset TestLifeEvent,FinEnv)->Bool
andTests fs x y = not (any not (map (\f -> f x y) fs)) 

almostEqual::Double->Double->Double->Bool
almostEqual eps x y = abs (x - y) < eps

almostEqualMV::ExchangeRateFunction->MoneyValue->MoneyValue->Bool
almostEqualMV e (MoneyValue x ccy) mv2 = almostEqual 1.0 x y where
  (MoneyValue y _) = MV.convert mv2 ccy e 
  

isFinalNW::MoneyValue->FMCTestF
isFinalNW nw _ (cs,fe) = 
  let e = fe ^. feExchange
      ps = cs ^. csMC.mcsPathSummary
  in case ps of
    FinalNW x -> almostEqualMV e nw x  
    ZeroNW _ -> False
    

isFinalAcctValue::AccountName->MoneyValue->FMCTestF
isFinalAcctValue acctName amt _ (cs,fe) = f where
  e = fe ^. feExchange
  acctE = getAccount acctName (cs ^. csMC.mcsBalanceSheet)
  f = case acctE of
    Right acct -> almostEqualMV e amt (accountValue acct e)
    Left _ -> False
    
isBankruptBy::Year->FMCTestF    
isBankruptBy d _ (cs,_) =  
  let ps = cs ^. csMC.mcsPathSummary
  in case ps of 
    ZeroNW day -> day <= d
    FinalNW _ -> False
    
isBankruptBetween::Year->Year->FMCTestF
isBankruptBetween before after = notTest (isBankruptBy before) `andTest` isBankruptBy after


assetTests = ConfigTests "Configs/Tests/AssetTestConfigurations.xml" 
             [
               ("BankTest",10,[FMCTest "Bank Interest" (isFinalNW (MoneyValue 110462 USD))]),
               ("BankTest",20,[FMCTest "Failure" (notTest $ isFinalNW (MoneyValue 130000 USD))]), 
               ("StockFundTest",10,[FMCTest "Stock return" (isFinalNW (MoneyValue 162889 USD))]),
               ("MixedFundTest",10,[FMCTest "Mixed return" (isFinalNW (MoneyValue 148024 USD))]),
               ("GuaranteedFundTest",10,[FMCTest "Guaranteed 5% return" (isFinalNW (MoneyValue 162889 USD))]),
               ("MortgageFailTest1",40,[FMCTest "MtgeBust1" (isBankruptBetween 2025 2026)]),
               ("MortgageFailTest2",40,[FMCTest "MtgeBust2" (isBankruptBetween 2025 2026)]),
               ("MortgageTest",35,[FMCTest "Mortgage" (isFinalNW (MoneyValue 6744 USD))])
             ]
             
flowTests = ConfigTests "Configs/Tests/FlowTestConfigurations.xml"
            [
              ("SalaryTest",20,[FMCTest "Salary Only" (isFinalNW (MoneyValue 2400000 USD))]),
              ("SalaryTest",21,[FMCTest "Salary End" (isFinalNW (MoneyValue 2400000 USD))]),
              ("ExpenseTest",5,[FMCTest "Expense Only" (isFinalNW (MoneyValue 40000 USD))]),
              ("ExpenseTest",10,[FMCTest "Expense->Bust" (isBankruptBetween 2022 2023)]),
              ("InflationTest",2,[FMCTest "Inflation Test" (isFinalNW (MoneyValue 75880 USD))]),
              ("InflationTest",3,[FMCTest "Inflation Test2" (isFinalNW (MoneyValue 63639 USD))]),
              ("PaymentTest",1,[FMCTest "Payment Test 1" (isFinalNW (MoneyValue 100000 USD))]),
              ("PaymentTest",2,[FMCTest "Payment Test 2" (isFinalNW (MoneyValue 101000 USD))]),
              ("PaymentTest",3,[FMCTest "Payment Test 3" (isFinalNW (MoneyValue 102050 USD))]),
              ("FX PaymentTest",1,[FMCTest "FX Payment Test 1" (isFinalNW (MoneyValue 100000 USD))]),
              ("FX PaymentTest",2,[FMCTest "FX Payment Test 2" (isFinalNW (MoneyValue 101200 USD))]),
              ("FX PaymentTest",3,[FMCTest "FX Payment Test 3" (isFinalNW (MoneyValue 102400 USD))])
            ]
            
taxTests = ConfigTests "Configs/Tests/TaxTestConfigurations.xml"
           [
             ("10kTest_FedOnly",1,[FMCTest "Fed Tax On 10k" (isFinalNW (MoneyValue 8235 USD))]),
             ("40kTest_FedOnly",1,[FMCTest "Fed Tax On 40k" (isFinalNW (MoneyValue 31832 USD))]),
             ("1MMTest_FedOnly",1,[FMCTest "Fed Tax On 1MM" (isFinalNW (MoneyValue 638725 USD))]),
             ("10kTest_NYS",1,[FMCTest "Fed+NYS Tax On 10k" (isFinalNW (MoneyValue 7875 USD))]),
             ("40kTest_NYS",1,[FMCTest "Fed+NYS Tax On 40k" (isFinalNW (MoneyValue 30188 USD))]),
             ("1MMTest_NYS",1,[FMCTest "Fed_NYS Tax On 1MM" (isFinalNW (MoneyValue 598304 USD))]),
             ("10kTest_NYC",1,[FMCTest "Fed+NYS+NYC Tax On 10k" (isFinalNW (MoneyValue 7613 USD))]),
             ("40kTest_NYC",1,[FMCTest "Fed+NYS+NYC Tax On 40k" (isFinalNW (MoneyValue 29102 USD))]),
             ("1MMTest_NYC",1,[FMCTest "Fed_NYS+NYC Tax On 1MM" (isFinalNW (MoneyValue 575710 USD))]),
             ("DeductibleExpense",1,[FMCTest "Deductible Expense" (isFinalNW (MoneyValue 570318 USD))]),
             ("TaxTrade",1,[FMCTest "Tax Trade" (andTests [isFinalNW (MoneyValue 89149 USD),
                                                          isFinalAcctValue "Citibank" (MoneyValue 86436 USD),
                                                          isFinalAcctValue "Sweep" (MoneyValue 2713 USD)])]),         
             ("CapitalGainFedOnly",1,[FMCTest "Untaxed Capital Gain (Fed Only)" 
                                      (andTests [isFinalNW (MoneyValue 250000 USD),
                                                 isFinalAcctValue "Vanguard" (MoneyValue 50000 USD),
                                                 isFinalAcctValue "Bank" (MoneyValue 150000 USD),
                                                 isFinalAcctValue "Sweep" (MoneyValue 50000 USD)])]),
             ("CapitalGainFed+NYS",1,[FMCTest "Capital Gain Fed+NYS" 
                                      (andTests [isFinalNW (MoneyValue 247750 USD),
                                                 isFinalAcctValue "Vanguard" (MoneyValue 50000 USD),
                                                 isFinalAcctValue "Bank" (MoneyValue 150000 USD),
                                                 isFinalAcctValue "Sweep" (MoneyValue 47750 USD)])]),         
             ("CapitalGainFed+NYS+NYC",1,[FMCTest "Capital Gain (1MM income) Fed+NYS+NYC" 
                                      (andTests [isFinalNW (MoneyValue 818460 USD),
                                                 isFinalAcctValue "Vanguard" (MoneyValue 50000 USD),
                                                 isFinalAcctValue "Bank" (MoneyValue 150000 USD),
                                                 isFinalAcctValue "Sweep" (MoneyValue 618460 USD)])]),             
             ("CapitalGain_FundEarnings",1,[FMCTest "Capital Gain (Fund Earnings) Fed+NYS+NYC" (isFinalNW (MoneyValue 675110 USD))]),
             ("Rental",1,[FMCTest "Rental" (isFinalNW (MoneyValue 124924 USD))]),
             ("MortgageDeduction",1,[FMCTest "Mortgage Deduction" 
                                     (andTests [isFinalNW (MoneyValue 277360 USD),
                                                isFinalAcctValue "MortgageAccount" (MV.negate (MoneyValue 98525 USD))])])
             
           ]

ruleTests = ConfigTests "Configs/Tests/RuleTestConfigurations.xml"             
            [
              ("SweepTest",1,[FMCTest "Cash<->Inv Sweep Rule" 
                              (andTests [isFinalNW (MoneyValue 200000 USD),
                                         isFinalAcctValue "Citibank" (MoneyValue 50000 USD)])]),
              ("SweepTest",2,[FMCTest "Cash<->Inv Sweep Rule" 
                              (andTests [isFinalNW (MoneyValue 320000 USD),
                                         isFinalAcctValue "Citibank" (MoneyValue 170000 USD)])]),
              ("SweepTest",3,[FMCTest "Cash<->Inv Sweep Rule" 
                              (andTests [isFinalNW (MoneyValue 440000 USD),
                                         isFinalAcctValue "Citibank" (MoneyValue 200000 USD)])]),
              ("SweepTest",4,[FMCTest "Cash<->Inv Sweep Rule" 
                              (andTests [isFinalNW (MoneyValue 560000 USD),
                                         isFinalAcctValue "Citibank" (MoneyValue 200000 USD)])]),
              ("RequiredDistributionTest",1,[FMCTest "Required Distribution Rule"
                                             (andTests [isFinalNW (MoneyValue 150000 USD),
                                                        isFinalAcctValue "bank" (MoneyValue 50000 USD),
                                                        isFinalAcctValue "IRA"  (MoneyValue 100000 USD)])]),
              ("RequiredDistributionTest",3,[FMCTest "Required Distribution Rule"
                                             (andTests [isFinalNW (MoneyValue 150000 USD),
                                                        isFinalAcctValue "bank" (MoneyValue 55882 USD),
                                                        isFinalAcctValue "IRA"  (MoneyValue 94118 USD)])]),
              ("RequiredDistributionTest",4,[FMCTest "Required Distribution Rule"
                                             (andTests [isFinalNW (MoneyValue 150000 USD),
                                                        isFinalAcctValue "bank" (MoneyValue 61656 USD),
                                                        isFinalAcctValue "IRA"  (MoneyValue 88344 USD)])]),
              ("EmergencySellTest",1,[FMCTest "Emergency Sell Rule"
                                      (andTests [isFinalNW (MoneyValue 400000 USD),
                                                 isFinalAcctValue "Citibank" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Citibank2" (MoneyValue 100000 USD),
                                                 isFinalAcctValue "Retirement" (MoneyValue 200000 USD),
                                                 isFinalAcctValue "529" (MoneyValue 100000 USD)])]),
              ("EmergencySellTest",2,[FMCTest "Emergency Sell Rule"
                                      (andTests [isFinalNW (MoneyValue 300000 USD),
                                                 isFinalAcctValue "Citibank" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Citibank2" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Retirement" (MoneyValue 200000 USD),
                                                 isFinalAcctValue "529" (MoneyValue 100000 USD)])]),
              ("EmergencySellTest",3,[FMCTest "Emergency Sell Rule"
                                      (andTests [isFinalNW (MoneyValue 200000 USD),
                                                 isFinalAcctValue "Citibank" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Citibank2" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Retirement" (MoneyValue 200000 USD),
                                                 isFinalAcctValue "529" (MoneyValue 0 USD)])]),
              ("EmergencySellTest",4,[FMCTest "Emergency Sell Rule"
                                      (andTests [isFinalNW (MoneyValue 90000 USD),
                                                 isFinalAcctValue "Citibank" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Citibank2" (MoneyValue 0 USD),
                                                 isFinalAcctValue "Retirement" (MoneyValue 100000 USD),
                                                 isFinalAcctValue "529" (MoneyValue 0 USD)])])
              
            ]


lifeEventTests = ConfigTests "Configs/Tests/LifeEventTestConfigurations.xml"
            [
              ("BuyHouseTest",1,[FMCTest "Buy House (pre)" (isFinalNW (MoneyValue 200000 USD))]),
              ("BuyHouseTest",2,[FMCTest "Buy House (bought at beginning)" 
                                 (andTests [isFinalNW (MoneyValue 170985.32 USD),
                                           isFinalAcctValue "Citibank" (MoneyValue 68039.32 USD),
                                           isFinalAcctValue "home_account" (MoneyValue 102946 USD)])])
                                            
            ]

allTests = [assetTests,flowTests,taxTests,ruleTests,lifeEventTests]

succeeds n = TestInstance 
           { run = return $ Finished Pass, 
             name = n,
             tags = [],
             options = [],
             setOption = \_ _ -> Right $ succeeds n}
           
fails n failMsg = TestInstance 
           { run = return $ Finished $ Fail failMsg, 
             name = n,
             tags = [],
             options = [],
             setOption = \_ _ -> Right $ fails n failMsg}
                       

doTest::String->(CombinedState TestAsset TestLifeEvent,FinEnv)->Int->FMCTestF->IO Test
doTest testName (cs0,fe0) years f = do 
  let result = execOnePathPure leConverter cs0 fe0 1 years
  case result of     
    Left e -> do
      putStrLn ("Execution failed.  Error=" ++ show e) 
      return $ Test $ fails "<name>" ("Execution failed. Error=" ++ show e)
    Right x -> do
      putStrLn $ if f years x then "Pass" else "Fail (" ++ show x ++ ")"    
      return $ if f years x then Test $ succeeds testName else Test $ fails testName "test failed"
   
evalTest::FMCTestSet->IO [Test]
evalTest (ConfigTests cFile testSets) = do
  putStrLn ("\nRunning tests from config file=" ++ cFile)
  (configInfo, configMap) <- loadConfigurations ccs Nothing baseParsers (C.UnparsedFile cFile)
  let f (cfgName,years,testl) = do
        (_,fe0,cs0) <- eitherToIO $ buildInitialStateFromConfig configInfo configMap cfgName
        let csHist = cs0 & (csNeedHistory .~ True)
            g (FMCTest testName testF) = do
              putStr ("Test=" ++ testName ++ " (" ++ show years ++ " yrs)->")
              doTest testName (csHist,fe0) years testF
        mapM g testl
  x <- mapM f testSets
  return $ concat x
  
evalTest (StateTests (cs0,fe0) testSets) = do
  putStrLn "\n Running tests from given config."
  let csHist = cs0 & (csNeedHistory .~ True)
      f (years,testl) = do
        let g (FMCTest testName testF) = do
              putStr ("Test=" ++ testName ++ " (" ++ show years ++ " yrs)->")
              doTest testName (csHist,fe0) years testF
        mapM g testl
  x <- mapM f testSets
  return $ concat x

runTests::[FMCTestSet]->IO [[Test]]
runTests = mapM evalTest 


tests::IO [[Test]]
tests = runTests allTests

mainTest:: IO ()
mainTest = do
  _ <- runTests allTests
  return ()
