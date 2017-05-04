{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module FinancialMC.Core.Engine 
       (
         RandomSeed
       , PathSummaryAndSeed (..)
       , HasPathSummaryAndSeed (..)       
       , doOneYear
       , doPath
       , execOnePathIO
       , execOnePathPure
       , doPathsIO
       , doPaths
       , EngineC
         -- Only for Benchmarking
       ) where

import           FinancialMC.Core.Evolve (applyAccums,applyFlows)
import           FinancialMC.Core.Result (ResultT,Result(Result),runResultT)
import           FinancialMC.Core.Asset (IsAsset)
import           FinancialMC.Core.Flow (IsFlow)
import           FinancialMC.Core.Rule (IsRule(..),RuleOutput(..),RuleWhen(..))
import           FinancialMC.Core.Rates (IsRateModel)
import           FinancialMC.Core.TradingTypes (Transaction(..))
import           FinancialMC.Core.AssetTrading (tradeAccount)
import           FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..),FinState,HasFinState(..),
                                                   updateExchangeRateFunction,
                                                   addToAccumulator,zeroAccumulator,addCashFlow)

import FinancialMC.Core.FinApp (LoggableStepApp(..),zoomStepApp,magnifyStepApp,toStepApp,LoggablePathApp(..),
                                execPPathApp,execPathApp,toPathApp,LogLevel(..),
                                zoomPathAppS,taxDataApp2StepAppFSER)

import FinancialMC.Core.MCState (CombinedState,HasCombinedState(..),HasMCState(..),PathSummary(..),
                                computeFlows,summarize,evolveMCS,addFlow,getAccount,insertAccount)


import           FinancialMC.Core.LifeEvent (IsLifeEvent(..),LifeEventConverters,
                                             lifeEventYear,lifeEventName,LifeEventOutput(..))
import           FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),HasMoneyValue(..),ExchangeRateFunction)
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Tax (updateTaxRules,carryForwardTaxData,fullTaxCV,TaxData)
import           FinancialMC.Core.Rates (applyModel,rateRequest,RateTag(Inflation),InflationType(TaxBracket))
import           FinancialMC.Core.Utilities (readOnly,FMCException(..))

import           Prelude hiding (log)

import qualified Data.Text as T

import           System.Random.Mersenne.Pure64 (PureMT,randomWord64,pureMT)
import           Control.Lens (use,zoom,view,magnify,(^.),(+=),(.=),(%=),Lens',_2,makeClassy)
import           Control.Monad (foldM,when,unless)
import           Control.Monad.Reader (ReaderT,ask)
import           Control.Monad.State.Strict (MonadState,get,execState)
import           Control.Monad.Morph (lift,hoist)
import           Control.Monad.Catch (throwM,MonadThrow)
import           Control.Exception (SomeException)
import qualified Control.Monad.Parallel as CMP
import           Control.DeepSeq (deepseq)
import           Control.Parallel.Strategies (using,parList,rseq,rpar)
import           Data.Word (Word64)
--import qualified Data.Vector as V

-- types for analysis
type RandomSeed = Word64
data PathSummaryAndSeed = PathSummaryAndSeed { _psasSummary :: !PathSummary, _psasSeed :: !RandomSeed }
makeClassy ''PathSummaryAndSeed

-- the "drop 1" is to get rid of the (0,pMT) entry
makeAllTheSeeds::PureMT->[RandomSeed]
makeAllTheSeeds pMT = drop 1 . fst . unzip $ iterate (randomWord64 . snd) (0,pMT)

getNSeeds::PureMT->Int->[RandomSeed]
getNSeeds pMT n = take n $ makeAllTheSeeds pMT

{-
getNSeedsStrict::PureMT->Int->[RandomSeed]
getNSeedsStrict pMT n = (head l)  `deepseq` l where
 l = getNSeeds pMT n
-}

type EngineC a fl le ru rm= (IsLifeEvent le, Show le,
                           IsAsset a, Show a,
                           IsFlow fl, Show fl,
                           IsRule ru, Show ru,
                           IsRateModel rm)

execOnePathIO::EngineC a fl le ru rm=>LifeEventConverters a fl le->[LogLevel]->
  CombinedState a fl le ru->FinEnv rm->RandomSeed->Int->IO (CombinedState a fl le ru,FinEnv rm)
execOnePathIO convertLE logDetails cs fe seed years = do
  let newSource = pureMT seed
  execPPathApp (doPath convertLE newSource years) logDetails cs fe  
  
doPathsIO::EngineC a fl le ru rm=>
           LifeEventConverters a fl le->[LogLevel]->Bool->CombinedState a fl le ru->
           FinEnv rm->Bool->PureMT->Int->Int->IO [PathSummaryAndSeed] --[(PathSummary,RandomSeed)]
doPathsIO convertLE logDetails showFinalStates cs0 fe0 singleThreaded pMT yearsPerPath paths = do
  let seeds = getNSeeds pMT paths
  let g seed = do
        (cs',_) <- execOnePathIO convertLE logDetails cs0 fe0 seed yearsPerPath
        when showFinalStates $ print cs'
        let q = cs' ^. csMC.mcsPathSummary 
        q `seq` (return $ PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
  if showFinalStates || singleThreaded || not (null logDetails) --parallelism doesn't play well with logging
    then mapM g seeds 
    else CMP.mapM g seeds


execOnePathPure::EngineC a fl le ru rm=>LifeEventConverters a fl le->CombinedState a fl le ru->
                 FinEnv rm->RandomSeed->Int->Either SomeException (CombinedState a fl le ru,FinEnv rm)  
execOnePathPure convertLE cs fe seed years = do
  let newSource = pureMT seed
  execPathApp (doPath convertLE newSource years) cs fe
  
doPaths::EngineC a fl le ru rm=>
         LifeEventConverters a fl le->CombinedState a fl le ru->
         FinEnv rm->Bool->PureMT->Int->Int->Either SomeException [PathSummaryAndSeed]
doPaths convertLE cs0 fe0 singleThreaded pMT yearsPerPath paths = do  
  let seeds =  getNSeeds pMT paths
      g::RandomSeed->Either SomeException PathSummaryAndSeed
      g seed = do
        (cs',_) <-  execOnePathPure convertLE cs0 fe0 seed yearsPerPath
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` Right (PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)

      eMap = seeds `deepseq` if singleThreaded then g <$> seeds 
                             else g <$> seeds `using` parList rseq
  sequence eMap
  
    
doPath::(EngineC a fl le ru rm,LoggablePathApp (CombinedState a fl le ru) (FinEnv rm) app)=>
  LifeEventConverters a fl le->PureMT->Int->app PureMT
doPath convertLE pMT years = foldM (\a _->doOneStepOnPath convertLE a) pMT [1..years]


lensFinEnv::Lens' (s,FinEnv rm) (FinEnv rm)
lensFinEnv = _2

lensToEmpty::Lens' a ()
lensToEmpty q x = const x <$> q ()

doOneStepOnPath::(EngineC a fl le ru rm,LoggablePathApp (CombinedState a fl le ru) (FinEnv rm) app)=>
  LifeEventConverters a fl le->PureMT->app PureMT
doOneStepOnPath convertLE pMT = pathLift $ do
  let feUpdater = toPathApp . zoom lensFinEnv
  newPMT <- feUpdater $ do
    x<-updateRates' pMT
    updateExchangeRateFunction
    return x
  step2Path $ do
    (inFlow,outFlow) <- zoomStepApp (csMC.mcsCashFlows) $ computeFlows 
    (tax,effRate)<- doOneYear convertLE
    summarize inFlow outFlow tax effRate 
  feUpdater updateCurrentDate
  zoomPathAppS lensToEmpty updateTaxBrackets --fix??
  return $! newPMT
  
updateCurrentDate::MonadState (FinEnv rm) m=>m ()
updateCurrentDate = feCurrentDate += 1

updateRates'::IsRateModel rm=>(MonadState (FinEnv rm) app,MonadThrow app)=>PureMT->app PureMT
updateRates' pMT = do
  rates <- use feRates
  model <- use feRateModel
  (newModel,(newRates,newPMT)) <- applyModel (rates,pMT) model
  feRates .= newRates
  feRateModel .= newModel
  return $! newPMT


updateTaxBrackets::LoggablePathApp () (FinEnv rm) app=>app ()
updateTaxBrackets = pathLift $ do
  taxBracketInflationRate <- toPathApp . readOnly . magnify (lensFinEnv.feRates) $ rateRequest (Inflation TaxBracket)
  (lensFinEnv.feTaxRules) %= flip updateTaxRules taxBracketInflationRate  

{-
updateTaxBrackets'::(MonadState FinEnv m, MonadThrow m)=>m ()
updateTaxBrackets' = do
  taxBracketInflationRate <- readOnly . zoom feRates $ rateRequest (Inflation TaxBracket) 
  feTaxRules %= flip updateTaxRules taxBracketInflationRate
-}

doOneYear::(EngineC a fl le ru rm,LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>
  LifeEventConverters a fl le->app (MoneyValue,Double)
doOneYear convertLE = do
  year <- view feCurrentDate
  stepLog Debug ("Beginning " ++ show year ++ ". Doing life events...")
  doLifeEvents convertLE
  stepLog Debug "Evolving assets and cashflows..."
  evolveMCS
  fs<-use csFinancial
  stepLog Debug ("Accumulators=" ++ show (fs ^. fsAccumulators))
  doRules BeforeTax
  (tax,effRate)<-doTax
  doSweepTrades  
  doRules AfterSweep
  doChecks
  stepLog Debug ("Completed " ++ show year ++ ".")
  return  (tax,effRate)


checkTrue::MonadThrow m=>Bool->String->m ()
checkTrue cond errMsg = unless cond $ throwM (Other errMsg)

checkFalse::MonadThrow m=>Bool->String->m ()
checkFalse cond errMsg = when cond $ throwM (Other errMsg)

checkEndingCash::LoggableStepApp MoneyValue ExchangeRateFunction app=> app ()  
checkEndingCash = do
  e <- ask
  cash <- get
  checkFalse (MV.gt e cash (MoneyValue 1 (cash^.mCurrency))) ("Ending cash position " ++ show cash ++ " > 0")

doChecks::LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app=> app ()  
doChecks = do
  stepLift . zoomStepApp (csFinancial.fsCashFlow) . magnifyStepApp feExchange $ checkEndingCash

morphInnerRuleStack::LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app=>ReaderT FinState (ReaderT (FinEnv rm) (Either SomeException)) b->app b
morphInnerRuleStack = stepLift.toStepApp.zoom csFinancial.readOnly
  
morphResultStack::(LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>
                  ResultT o (ReaderT FinState (ReaderT (FinEnv rm) (Either SomeException))) b->ResultT o app b  
morphResultStack =  hoist morphInnerRuleStack 
  
doRules::(EngineC a fl le ru rm, LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>RuleWhen->app ()  
doRules w = do
  mcs <- use csMC
  let isRuleNow r = ruleWhen r == w
      liveRules = filter isRuleNow (mcs ^. mcsRules)
      getA name = getAccount name (mcs ^. mcsBalanceSheet) 
      f::(IsRule ru,IsRateModel rm)=>LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app=>ru->ResultT RuleOutput app () 
      f r = do
        lift $ stepLog Debug ("Doing " ++ show (ruleName r)) 
        morphResultStack (doRule r getA)
  stepLog Debug ("Doing " ++ show w ++ " rules.")
  Result _ ruleOutput <- runResultT $ mapM_ f liveRules -- can/should move zoom/hoist to here?  Does it matter?
  doRuleResult ruleOutput
  
doRuleResult::(IsAsset a,Show a,
               LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>RuleOutput->app ()
doRuleResult (RuleOutput trades accs) = do
  stepLog Debug ("Resulting in trades:" ++ show trades ++ " and accums=" ++ show accs)
  stepLift . magnifyStepApp feExchange $ do
    zoomStepApp csFinancial $ applyAccums accs
    doTransactions trades

doLifeEvents::forall a fl le ru rm app.(EngineC a fl le ru rm,
                                        LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>
              LifeEventConverters a fl le->app ()  
doLifeEvents convertLE = do
  fe <- ask
  mcs <- use csMC
  let curDate = fe ^. feCurrentDate
      happeningThisYear le = (lifeEventYear le == curDate)
      liveEvents = filter happeningThisYear (mcs ^. mcsLifeEvents)
      getA name = getAccount name (mcs ^. mcsBalanceSheet) 
      f::le->ResultT (LifeEventOutput a fl) app () 
      f x = do
        lift $ stepLog Debug ("Doing " ++ show (lifeEventName x))
        morphResultStack (doLifeEvent x convertLE getA)
  stepLog Debug ("Doing " ++ show curDate ++ " life events.")
  Result _ results <- runResultT $ mapM_ f liveEvents
  doLifeEventResult results

doLifeEventResult::(IsFlow fl, LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>LifeEventOutput a fl->app ()
doLifeEventResult (LifeEventOutput newAccounts newFlows) = do
  csMC.mcsBalanceSheet %= execState (mapM_ insertAccount newAccounts)
  csMC.mcsCashFlows %= execState (mapM_ addFlow newFlows) 
  
  
doTax::(EngineC a fl le ru rm, LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>app (MoneyValue,Double)
doTax = stepLift $ do
  zoomStepApp csFinancial $ do 
    (taxPre,_) <- zoomStepApp fsTaxData $ computeTax -- get amount   
    toStepApp $ addToAccumulator (T.pack "TaxOwed") taxPre  -- store amount for tax sweep rule.  Ick.
  doTaxTrade
  zoomStepApp csFinancial $ do
    (tax',rate') <- zoomStepApp fsTaxData $ computeTax -- recompute tax since trade may have incurred cap gains
    magnifyStepApp feExchange $ payTax tax'  -- pay tax, zero tax counters and carry forward losses
    toStepApp $ zeroAccumulator (T.pack "TaxOwed")
    return (tax',rate')


doTaxTrade::(IsAsset a,Show a,IsRule ru, IsRateModel rm,
              LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>app ()
doTaxTrade = stepLift $ do
  mcs <- use csMC
  let getA name = getAccount name ( mcs ^. mcsBalanceSheet) 
  curPos <- use (csFinancial.fsCashFlow)
  stepLog Debug ("Current cash on hand is " ++ show curPos)
  stepLog Debug "Generating tax trade, if necessary"
  Result _ result <- morphInnerRuleStack $ runResultT (doRule (mcs ^. mcsTaxTrade) getA)
  doRuleResult result

    
payTax::LoggableStepApp FinState ExchangeRateFunction app=>MoneyValue->app ()
payTax tax = do
  let taxPmt = MV.negate tax
  stepLift $ toStepApp (addCashFlow taxPmt) >> taxDataApp2StepAppFSER carryForwardTaxData
  stepLog Debug ("Paid Tax of " ++ show tax)

  
doSweepTrades::(EngineC a fl le ru rm, LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) app)=>app ()
doSweepTrades = stepLift $ do
  mcs <- use csMC
  let getA name = getAccount name ( mcs ^. mcsBalanceSheet) 
  Result _ result <- morphInnerRuleStack $ runResultT (doRule (mcs ^. mcsSweep) getA)
  doRuleResult result


doAccountTransaction::(IsAsset a,Show a,
                       LoggableStepApp (CombinedState a fl le ru) ExchangeRateFunction app)=>Transaction->app ()
doAccountTransaction tr@(Transaction target typ amt)= stepLift $ do
  mcs <- use csMC
  let balanceSheet = mcs ^. mcsBalanceSheet     
  Result after flows <- toStepApp . lift $ do    
    acct <- lift $ getAccount target balanceSheet
    runResultT $ tradeAccount acct typ amt   
    
  zoomStepApp csFinancial $ applyFlows flows
  zoomStepApp (csMC.mcsBalanceSheet) $ insertAccount after 
    
  fs <- use csFinancial
  stepLog Debug ("Current net cash flow is " ++ show (fs ^. fsCashFlow))
  stepLog Debug ("Did Transaction (" ++ show tr ++ ")")
  stepLog Debug ("With resulting account " ++ show after ++ " and flows " ++ show flows)


isNonZeroTransaction::Transaction->Bool
isNonZeroTransaction (Transaction _ _ (MoneyValue x _)) = x/=0

doTransactions::(IsAsset a,Show a,
                 LoggableStepApp (CombinedState a fl le ru) ExchangeRateFunction app)=>[Transaction]->app ()
doTransactions ts = stepLift $ mapM_ doAccountTransaction (filter isNonZeroTransaction ts)

computeTax::LoggableStepApp TaxData (FinEnv rm) app=>app (MoneyValue,Double)  -- main body now in Tax.hs
computeTax = stepLift $ do 
  tr <- view feTaxRules
  td <- get
  (total,rate) <- toStepApp . lift . (magnify feExchange) $ fullTaxCV tr td 
  stepLog Debug ("Computed tax of " ++ show total ++ " (eff rate of " ++ show (100*rate) ++ "%) given tax data: " ++ show td)
  return  (total,rate)

