{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FinancialMC.Core.Engine 
       (
         doOneYear,
         doPath,
         execOnePathIO,execOnePathPure,
         doPathsIO,doPaths
         -- Only for Benchmarking
       , 
       ) where

import           FinancialMC.Core.Evolve (Evolvable,applyAccums,applyFlows)
import           FinancialMC.Core.Result (ResultT,Result(Result),runResultT)
import           FinancialMC.Core.Asset (IsAsset)
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

import           FinancialMC.Core.Rule (Rule,IsRule(..),RuleOutput(..),RuleWhen(..))
import           FinancialMC.Core.LifeEvent (IsLifeEvent(..),LifeEventOutput(..))
import           FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),HasMoneyValue(..),ExchangeRateFunction)
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Tax (updateTaxRules,carryForwardTaxData,fullTaxCV,TaxData)
import           FinancialMC.Core.Rates (applyModel,rateRequest,RateTag(Inflation),InflationType(TaxBracket))
import           FinancialMC.Core.Utilities (readOnly,FMCException(..))

import           Prelude hiding (log)

import qualified Data.Text as T

import           System.Random.Mersenne.Pure64 (PureMT,randomWord64,pureMT)
import           Control.Lens (use,zoom,view,magnify,(^.),(+=),(.=),(%=),Lens',_2)
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

-- the "drop 1" is to get rid of the (0,pMT) entry
makeAllTheSeeds::PureMT->[Word64]
makeAllTheSeeds pMT = drop 1 . fst . unzip $ iterate (randomWord64 . snd) (0,pMT)

getNSeeds::PureMT->Int->[Word64]
getNSeeds pMT n = take n $ makeAllTheSeeds pMT

getNSeedsStrict::PureMT->Int->[Word64]
getNSeedsStrict pMT n = (head l)  `deepseq` l where
 l = getNSeeds pMT n

execOnePathIO::(IsLifeEvent le, IsAsset a, Show a)=>(AssetType le -> a) -> [LogLevel]->CombinedState a le->FinEnv->Word64->Int->IO (CombinedState a le,FinEnv)
execOnePathIO convertLE logDetails cs fe seed years = do
  let newSource = pureMT seed
  execPPathApp (doPath convertLE newSource years) logDetails cs fe  
  
doPathsIO::(IsLifeEvent le, Show le, IsAsset a,Show a)=>
           (AssetType le->a)->[LogLevel]->Bool->CombinedState a le->FinEnv->Bool->PureMT->Int->Int->IO [(PathSummary,Word64)]
doPathsIO convertLE logDetails showFinalStates cs0 fe0 singleThreaded pMT yearsPerPath paths = do
  let seeds = getNSeeds pMT paths
  let g seed = do
        (cs',_) <- execOnePathIO convertLE logDetails cs0 fe0 seed yearsPerPath
        when showFinalStates $ print cs'
        let q = cs' ^. csMC.mcsPathSummary 
        q `seq` return (cs' ^. csMC.mcsPathSummary,seed)
  if showFinalStates || singleThreaded || not (null logDetails) --parallelism doesn't play well with logging
    then mapM g seeds 
    else CMP.mapM g seeds


execOnePathPure::(IsLifeEvent le, IsAsset a, Show a)=>(AssetType le->a)->CombinedState a le->FinEnv->Word64->Int->Either SomeException (CombinedState a le,FinEnv)  
execOnePathPure convertLE cs fe seed years = do
  let newSource = pureMT seed
  execPathApp (doPath convertLE newSource years) cs fe
  
doPaths::(IsLifeEvent le, Show le, IsAsset a, Show a)=>
         (AssetType le->a)->CombinedState a le->FinEnv->Bool->PureMT->Int->Int->Either SomeException [(PathSummary,Word64)]
doPaths convertLE cs0 fe0 singleThreaded pMT yearsPerPath paths = do  
  let seeds =  getNSeeds pMT paths
      g::Word64->Either SomeException (PathSummary,Word64)
      g seed = do
        (cs',_) <-  execOnePathPure convertLE cs0 fe0 seed yearsPerPath
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` Right (cs' ^. csMC.mcsPathSummary,seed)

      eMap = seeds `deepseq` if singleThreaded then g <$> seeds 
                             else g <$> seeds `using` parList rseq
  sequence eMap
  
    
doPath::(IsLifeEvent le, IsAsset a,Show a)=>LoggablePathApp (CombinedState a le) FinEnv app=>(AssetType le -> a) -> PureMT->Int->app PureMT
doPath convertLE pMT years = foldM (\a _->doOneStepOnPath convertLE a) pMT [1..years]


lensFinEnv::Lens' (s,FinEnv) FinEnv
lensFinEnv = _2

lensToEmpty::Lens' a ()
lensToEmpty q x = const x <$> q ()

doOneStepOnPath::(IsLifeEvent le, IsAsset a,Show a,LoggablePathApp (CombinedState a le) FinEnv app)=>(AssetType le -> a)->PureMT->app PureMT
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
  
updateCurrentDate::MonadState FinEnv m=>m ()
updateCurrentDate = feCurrentDate += 1

updateRates'::(MonadState FinEnv app,MonadThrow app)=>PureMT->app PureMT
updateRates' pMT = do
  rates <- use feRates
  model <- use feRateModel
  (newModel,(newRates,newPMT)) <- applyModel (rates,pMT) model
  feRates .= newRates
  feRateModel .= newModel
  return $! newPMT


updateTaxBrackets::LoggablePathApp () FinEnv app=>app ()
updateTaxBrackets = pathLift $ do
  taxBracketInflationRate <- toPathApp . readOnly . magnify (lensFinEnv.feRates) $ rateRequest (Inflation TaxBracket)
  (lensFinEnv.feTaxRules) %= flip updateTaxRules taxBracketInflationRate  

{-
updateTaxBrackets'::(MonadState FinEnv m, MonadThrow m)=>m ()
updateTaxBrackets' = do
  taxBracketInflationRate <- readOnly . zoom feRates $ rateRequest (Inflation TaxBracket) 
  feTaxRules %= flip updateTaxRules taxBracketInflationRate
-}

doOneYear::(IsLifeEvent le,IsAsset a,Show a,LoggableStepApp (CombinedState a le) FinEnv app)=>(AssetType le->a)->app (MoneyValue,Double)
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

doChecks::LoggableStepApp (CombinedState a le) FinEnv app=> app ()  
doChecks = do
  stepLift . zoomStepApp (csFinancial.fsCashFlow) . magnifyStepApp feExchange $ checkEndingCash

morphInnerRuleStack::LoggableStepApp (CombinedState a le) FinEnv app=>ReaderT FinState (ReaderT FinEnv (Either SomeException)) b->app b
morphInnerRuleStack = stepLift.toStepApp.zoom csFinancial.readOnly
  
morphResultStack::(Monoid o,LoggableStepApp (CombinedState a le) FinEnv app)=>
                  ResultT o (ReaderT FinState (ReaderT FinEnv (Either SomeException))) b->ResultT o app b  
morphResultStack =  hoist morphInnerRuleStack 
  
doRules::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) FinEnv app=>RuleWhen->app ()  
doRules w = do
  mcs <- use csMC
  let isRuleNow r = ruleWhen r == w
      liveRules = filter isRuleNow (mcs ^. mcsRules)
      getA name = getAccount name (mcs ^. mcsBalanceSheet) 
      f::LoggableStepApp (CombinedState a le) FinEnv app=>Rule->ResultT RuleOutput app () 
      f r = do
        lift $ stepLog Debug ("Doing " ++ show (ruleName r)) 
        morphResultStack (doRule r getA)
  stepLog Debug ("Doing " ++ show w ++ " rules.")
  Result _ ruleOutput <- runResultT $ mapM_ f liveRules -- can/should move zoom/hoist to here?  Does it matter?
  doRuleResult ruleOutput
  
doRuleResult::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) FinEnv app=>RuleOutput->app ()
doRuleResult (RuleOutput trades accs) = do
  stepLog Debug ("Resulting in trades:" ++ show trades ++ " and accums=" ++ show accs)
  stepLift . magnifyStepApp feExchange $ do
    zoomStepApp csFinancial $ applyAccums accs
    doTransactions trades

doLifeEvents::forall a le app.(IsAsset a,IsLifeEvent le,LoggableStepApp (CombinedState a le) FinEnv app)=>(AssetType le->a)->app ()  
doLifeEvents convertLE = do
  fe <- ask
  mcs <- use csMC
  let curDate = fe ^. feCurrentDate
      happeningThisYear le = (lifeEventYear le == curDate)
      liveEvents = filter happeningThisYear (mcs ^. mcsLifeEvents)
      getA name = getAccount name (mcs ^. mcsBalanceSheet) 
      f::LoggableStepApp (CombinedState a le) FinEnv app=>le->ResultT (LifeEventOutput a) app () 
      f x = do
        lift $ stepLog Debug ("Doing " ++ show (lifeEventName x))
        morphResultStack (doLifeEvent x convertLE getA)
  stepLog Debug ("Doing " ++ show curDate ++ " life events.")
  Result _ results <- runResultT $ mapM_ f liveEvents
  doLifeEventResult results

doLifeEventResult::LoggableStepApp (CombinedState a le) FinEnv app=>LifeEventOutput a->app ()
doLifeEventResult (LifeEventOutput newAccounts newFlows) = do
  csMC.mcsBalanceSheet %= execState (mapM_ insertAccount newAccounts)
  csMC.mcsCashFlows %= execState (mapM_ addFlow newFlows) 
  
  
doTax::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) FinEnv app=>app (MoneyValue,Double)
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


doTaxTrade::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) FinEnv app=>app ()
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

  
doSweepTrades::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) FinEnv app=>app ()
doSweepTrades = stepLift $ do
  mcs <- use csMC
  let getA name = getAccount name ( mcs ^. mcsBalanceSheet) 
  Result _ result <- morphInnerRuleStack $ runResultT (doRule (mcs ^. mcsSweep) getA)
  doRuleResult result


doAccountTransaction::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) ExchangeRateFunction app=>Transaction->app ()
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

doTransactions::(IsAsset a,Show a)=>LoggableStepApp (CombinedState a le) ExchangeRateFunction app=>[Transaction]->app ()
doTransactions ts = stepLift $ mapM_ doAccountTransaction (filter isNonZeroTransaction ts)

computeTax::LoggableStepApp TaxData FinEnv app=>app (MoneyValue,Double)  -- main body now in Tax.hs
computeTax = stepLift $ do 
  tr <- view feTaxRules
  td <- get
  (total,rate) <- toStepApp . lift . (magnify feExchange) $ fullTaxCV tr td 
  stepLog Debug ("Computed tax of " ++ show total ++ " (eff rate of " ++ show (100*rate) ++ "%) given tax data: " ++ show td)
  return  (total,rate)

