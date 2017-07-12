{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
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

import           FinancialMC.Core.Asset           (IsAsset)
import           FinancialMC.Core.AssetTrading    (tradeAccount)
import           FinancialMC.Core.Evolve          (Evolvable, applyAccums,
                                                   applyFlows)
import           FinancialMC.Core.FinancialStates (FinEnv, FinState,
                                                   HasAccumulators,
                                                   HasCashFlow (cashFlow),
                                                   HasFinEnv (..),
                                                   HasFinState (..),
                                                   ReadsAccumulators,
                                                   ReadsFinEnv (getFinEnv),
                                                   ReadsFinState (getFinState),
                                                   addCashFlow,
                                                   addToAccumulator,
                                                   updateExchangeRateFunction,
                                                   zeroAccumulator)
import           FinancialMC.Core.Flow            (IsFlow)
import           FinancialMC.Core.Rates           (IsRateModel, Rate,
                                                   ReadsRateTable)
import           FinancialMC.Core.Result          (Result (Result), ResultT,
                                                   runResultT)
import           FinancialMC.Core.Rule            (IsRule (..), RuleAppC,
                                                   RuleOutput (..),
                                                   RuleWhen (..))
import           FinancialMC.Core.TradingTypes    (Transaction (..))

import           FinancialMC.Core.FinApp          (LogLevel (..),
                                                   Loggable (log),
                                                   PathState (..),
                                                   execPPathStack,
                                                   execPathStack)

import           FinancialMC.Core.MCState         (CombinedState,
                                                   HasBalanceSheet (..),
                                                   HasCashFlows (..),
                                                   HasCombinedState (..),
                                                   HasMCState (..),
                                                   PathSummary (..),
                                                   ReadsCombinedState (getCombinedState),
                                                   ReadsMCState (getMCState),
                                                   addFlow, computeFlows,
                                                   evolveMCS, getAccount,
                                                   insertAccount, summarize)


import           FinancialMC.Core.LifeEvent       (IsLifeEvent (..),
                                                   LifeEventAppC,
                                                   LifeEventConverters,
                                                   LifeEventOutput (..),
                                                   lifeEventName, lifeEventYear)
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction,
                                                   HasMoneyValue (..),
                                                   MoneyValue (MoneyValue),
                                                   ReadsExchangeRateFunction (getExchangeRateFunction))
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Rates           (InflationType (TaxBracket),
                                                   RateTag (Inflation),
                                                   applyModel, rateRequest)
import           FinancialMC.Core.Tax             (HasTaxData,
                                                   ReadsTaxData (getTaxData),
                                                   TaxData, TaxDataAppC,
                                                   carryForwardTaxData,
                                                   fullTaxCV, updateTaxRules)
import           FinancialMC.Core.Utilities       (AsFMCException (..),
                                                   FMCException (..), readOnly)

import           Prelude                          hiding (log)

import qualified Data.Text                        as T

import           Control.DeepSeq                  (deepseq)
import           Control.Lens                     (Lens', magnify, makeClassy,
                                                   use, view, zoom, (%=), (+=),
                                                   (.=), (^.), _2)
import           Control.Monad                    (foldM, unless, when)
import           Control.Monad.Morph              (hoist, lift)
import qualified Control.Monad.Parallel           as CMP
import           Control.Monad.Reader             (ReaderT, ask)
import           Control.Monad.State.Strict       (MonadState, execState, get)
import           Control.Parallel.Strategies      (parList, rpar, rseq, using)
import           Data.Monoid                      ((<>))
import           Data.Word                        (Word64)
import           System.Random.Mersenne.Pure64    (PureMT, pureMT, randomWord64)

import           Control.Monad.Error.Lens         (throwing)
import           Control.Monad.Except             (MonadError)

-- types for analysis
type RandomSeed = Word64
data PathSummaryAndSeed = PathSummaryAndSeed { _psasSummary :: !PathSummary, _psasSeed :: !RandomSeed }
makeClassy ''PathSummaryAndSeed

-- the "drop 1" is to get rid of the (0,pMT) entry
makeAllTheSeeds :: PureMT -> [RandomSeed]
makeAllTheSeeds pMT = drop 1 . fst . unzip $ iterate (randomWord64 . snd) (0,pMT)

getNSeeds :: PureMT -> Int -> [RandomSeed]
getNSeeds pMT n = take n $ makeAllTheSeeds pMT

{-
getNSeedsStrict :: PureMT -> Int -> [RandomSeed]
getNSeedsStrict pMT n = (head l)  `deepseq` l where
 l = getNSeeds pMT n
-}

type EngineC a fl le ru rm= (IsLifeEvent le, Show le,
                             IsAsset a, Show a,
                             IsFlow fl, Show fl,
                             IsRule ru, Show ru,
                             IsRateModel rm)

type FullPathC s a fl le ru rm m = ( EngineC a fl le ru rm
                                   , Loggable m
                                   , MonadError FMCException m
                                   , MonadState s m
                                   , ReadsFinState s
                                   , ReadsTaxData s
                                   , ReadsFinEnv s rm
                                   , ReadsExchangeRateFunction s
                                   , Evolvable a
                                   , Evolvable fl
                                   , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
                                   , IsRateModel rm
                                   , HasAccumulators s
                                   , HasCashFlow s
                                   , HasBalanceSheet s a
                                   , HasTaxData s
                                   , HasMCState s a fl le ru
                                   , ReadsMCState s a fl le ru)

-- The IO versions are required for logging.
-- If we only use IO for entropy, config loading and result reporting, we can use the pure stack.
-- But to do realtime logging to stdout we need IO in the stack all the time.

--type EnginePathState a fl le ru rm = PathState (CombinedState a fl le ru) (FinEnv rm)

execOnePathIO :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> [LogLevel]
  -> CombinedState a fl le ru
  -> FinEnv rm
  -> RandomSeed
  -> Int
  -> IO (PathState (CombinedState a fl le ru) (FinEnv rm))
execOnePathIO convertLE logDetails cs fe seed years = do
  let newSource = pureMT seed
  execPPathStack (doPath convertLE newSource years) logDetails (PathState cs fe)

doPathsIO :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> [LogLevel]
  -> Bool
  -> CombinedState a fl le ru
  -> FinEnv rm
  -> Bool
  -> PureMT
  -> Int
  -> Int
  -> IO [PathSummaryAndSeed] --[(PathSummary,RandomSeed)]
doPathsIO convertLE logDetails showFinalStates cs0 fe0 singleThreaded pMT yearsPerPath paths = do
  let seeds = getNSeeds pMT paths
  let g seed = do
        PathState cs' _  <- execOnePathIO convertLE logDetails cs0 fe0 seed yearsPerPath
        when showFinalStates $ print cs'
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` (return $ PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
  if showFinalStates || singleThreaded || not (null logDetails) --parallelism doesn't play well with logging
    then mapM g seeds
    else CMP.mapM g seeds

execOnePathPure :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> CombinedState a fl le ru
  -> FinEnv rm
  -> RandomSeed
  -> Int
  -> Either FMCException (PathState (CombinedState a fl le ru) (FinEnv rm))
execOnePathPure convertLE cs fe seed years = do
  let newSource = pureMT seed
  execPathStack (doPath convertLE newSource years) (PathState cs fe)

doPaths :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> CombinedState a fl le ru
  -> FinEnv rm
  -> Bool
  -> PureMT
  -> Int
  -> Int
  -> Either FMCException [PathSummaryAndSeed]
doPaths convertLE cs0 fe0 singleThreaded pMT yearsPerPath paths = do
  let seeds =  getNSeeds pMT paths
      g seed = do
        PathState cs' _ <-  execOnePathPure convertLE cs0 fe0 seed yearsPerPath
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` Right (PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
      eMap = seeds `deepseq` if singleThreaded then g <$> seeds
                             else g <$> seeds `using` parList rseq
  sequence eMap

doPath :: ( EngineC a fl le ru rm
          , Loggable m
          , MonadError FMCException m
          , MonadState s m
          , ReadsFinState s
          , ReadsTaxData s
          , ReadsAccumulators s
          , HasFinEnv s rm
          , ReadsFinEnv s rm
          , ReadsExchangeRateFunction s
          , Evolvable a
          , Evolvable fl
          , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
          , IsRateModel rm
          , HasAccumulators s
          , HasCashFlow s
          , HasBalanceSheet s a
          , HasCashFlows s fl
          , HasTaxData s
          , HasCombinedState s a fl le ru
          , HasMCState s a fl le ru
          , ReadsMCState s a fl le ru)
  => LifeEventConverters a fl le
  -> PureMT
  -> Int
  -> m PureMT
doPath convertLE pMT years = foldM (\a _ -> doOneStepOnPath convertLE a) pMT [1..years]

lensFinEnv :: Lens' (s,FinEnv rm) (FinEnv rm)
lensFinEnv = _2

lensToEmpty :: Lens' a ()
lensToEmpty q x = const x <$> q ()

doOneStepOnPath :: ( EngineC a fl le ru rm
                   , Loggable m
                   , MonadError FMCException m
                   , MonadState s m
                   , ReadsFinState s
                   , ReadsTaxData s
                   , ReadsAccumulators s
                   , HasFinEnv s rm
                   , ReadsFinEnv s rm
                   , ReadsExchangeRateFunction s
                   , Evolvable a
                   , Evolvable fl
                   , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
                   , IsRateModel rm
                   , HasAccumulators s
                   , HasCashFlow s
                   , HasBalanceSheet s a
                   , HasCashFlows s fl
                   , HasTaxData s
                   , HasCombinedState s a fl le ru
                   , HasMCState s a fl le ru
                   , ReadsMCState s a fl le ru)
  => LifeEventConverters a fl le
  -> PureMT
  -> m PureMT
doOneStepOnPath convertLE pMT = do
  newPMT <- do
    x <- updateRates' pMT -- order matters here.  We need to update rates to get the updated exchange rate.
    updateExchangeRateFunction
    return x
  (inFlow, outFlow) <- computeFlows
  (tax, effRate) <- doOneYear convertLE
  summarize inFlow outFlow tax effRate
  updateCurrentDate
  updateTaxBrackets --fix??
  return $! newPMT

updateCurrentDate :: (MonadState s m, HasFinEnv s rm) => m ()
updateCurrentDate = (finEnv.feCurrentDate) += 1

updateRates' :: ( IsRateModel rm
                , MonadError FMCException m
                , MonadState s m
                , HasFinEnv s rm) => PureMT -> m PureMT
updateRates' pMT = do
  rates <- use $ finEnv.feRates
  model <- use $ finEnv.feRateModel
  (newModel,(newRates,newPMT)) <- applyModel (rates, pMT) model
  finEnv.feRates .= newRates
  finEnv.feRateModel .= newModel
  return $! newPMT


updateTaxBrackets :: ( MonadError FMCException m
                     , MonadState s m
                     , ReadsRateTable s Double
                     , HasFinEnv s rm) => m ()
updateTaxBrackets = do
  taxBracketInflationRate <- rateRequest (Inflation TaxBracket)
  (finEnv.feTaxRules) %= flip updateTaxRules taxBracketInflationRate

{-
updateTaxBrackets' :: (MonadState FinEnv m, MonadThrow m)=>m ()
updateTaxBrackets' = do
  taxBracketInflationRate <- readOnly . zoom feRates $ rateRequest (Inflation TaxBracket)
  feTaxRules %= flip updateTaxRules taxBracketInflationRate
-}

doOneYear :: ( EngineC a fl le ru rm
             , Loggable m
             , MonadError FMCException m
             , MonadState s m
             , ReadsFinState s
             , ReadsTaxData s
             , ReadsAccumulators s
             , ReadsFinEnv s rm
             , ReadsExchangeRateFunction s
             , Evolvable a
             , Evolvable fl
             , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
             , IsRateModel rm
             , HasAccumulators s
             , HasCashFlow s
             , HasBalanceSheet s a
             , HasCashFlows s fl
             , HasTaxData s
             , HasMCState s a fl le ru
             , ReadsMCState s a fl le ru)
  => LifeEventConverters a fl le
  -> m (MoneyValue,Double)
doOneYear convertLE = do
  year <- use $ getFinEnv.feCurrentDate
  log Debug ("Beginning " <> (T.pack $ show year) <> ". Doing life events...")
  doLifeEvents convertLE
  log Debug "Evolving assets and cashflows..."
  evolveMCS
  fs <- use getFinState
  log Debug ("Accumulators=" <> (T.pack $ show (fs ^. fsAccumulators)))
  doRules BeforeTax
  (tax, effRate) <- doTax
  doSweepTrades
  doRules AfterSweep
  doChecks
  log Debug ("Completed " <> (T.pack $ show year) <> ".")
  return (tax, effRate)

checkTrue :: MonadError FMCException m => Bool -> String -> m ()
checkTrue cond errMsg = unless cond $ throwing _Other (T.pack errMsg)

checkFalse :: MonadError FMCException m => Bool -> String -> m ()
checkFalse cond errMsg = when cond $ throwing _Other (T.pack errMsg)

checkEndingCash :: ( MonadError FMCException m
                   , MonadState s m
                   , ReadsExchangeRateFunction s
                   , ReadsFinState s) => m ()
checkEndingCash = do
  e <- use getExchangeRateFunction
  cash <- use $ getFinState.fsCashFlow
  checkFalse (MV.gt e cash (MoneyValue 1 (cash ^. mCurrency))) ("Ending cash position " ++ show cash ++ " > 0")

doChecks :: ( MonadError FMCException m
            , MonadState s m
            , ReadsExchangeRateFunction s
            , ReadsFinState s) => m ()
doChecks = do
  checkEndingCash

{-
morphInnerRuleStack :: LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) m
  => ReaderT FinState (ReaderT (FinEnv rm) (Either FMCException)) b -> m b
morphInnerRuleStack = zoomStep csFinancial . toStepApp . readOnly

morphResultStack :: LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) m
  => ResultT o (ReaderT FinState (ReaderT (FinEnv rm) (Either FMCException))) b -> ResultT o m b
morphResultStack =  hoist morphInnerRuleStack
-}

doRules :: ( Loggable m
           , EngineC a fl le ru rm
           , MonadError FMCException m
           , MonadState s m
           , ReadsFinState s
           , ReadsAccumulators s
           , ReadsFinEnv s rm
           , ReadsExchangeRateFunction s
           , HasAccumulators s
           , HasTaxData s
           , HasCashFlow s
           , HasBalanceSheet s a
           , ReadsMCState s a fl le ru)
  => RuleWhen -> m ()
doRules w = do
  rs <- use $ getMCState.mcsRules
  bs <- use $ getMCState.mcsBalanceSheet
  let isRuleNow r = ruleWhen r == w
      liveRules = filter isRuleNow rs
      getA name = getAccount name bs
--      f :: ( IsRule ru
--           , IsRateModel rm
--           , LoggableStepApp (CombinedState a fl le ru) (FinEnv rm) m) => ru -> ResultT RuleOutput m ()
      f r = do
        log Debug ("Doing " <> (T.pack $ show (ruleName r)))
        doRule r getA
  log Debug ("Doing " <> (T.pack $ show w) <> " rules.")
  Result _ ruleOutput <- runResultT $ mapM_ f liveRules -- can/should move zoom/hoist to here?  Does it matter?
  doRuleResult ruleOutput

doRuleResult :: ( IsAsset a
                , Show a
                , Loggable m
                , MonadError FMCException m
                , MonadState s m
                , HasTaxData s
                , ReadsExchangeRateFunction s
                , HasCashFlow s
                , HasBalanceSheet s a
                , HasAccumulators s)=> RuleOutput -> m ()
doRuleResult (RuleOutput trades accs) = do
  log Debug ("Resulting in trades:" <> (T.pack $ show trades) <> " and accums=" <> (T.pack $ show accs))
  applyAccums accs
  doTransactions trades

doLifeEvents :: ( EngineC a fl le ru rm
                , Loggable m
                , MonadError FMCException m
                , MonadState s m
                , HasBalanceSheet s a
                , HasCashFlows s fl
                , ReadsFinState s
                , ReadsFinEnv s rm
                , ReadsExchangeRateFunction s
                , ReadsMCState s a fl le ru)
  => LifeEventConverters a fl le -> m ()
doLifeEvents convertLE = do
  curDate <- use $ getFinEnv.feCurrentDate
  les <- use $ getMCState.mcsLifeEvents
  bs <- use $ getMCState.mcsBalanceSheet
  let happeningThisYear le = (lifeEventYear le == curDate)
      liveEvents = filter happeningThisYear les
      getA name = getAccount name bs
--      f :: le -> ResultT (LifeEventOutput a fl) m ()
      f x = do
        log Debug ("Doing " <> (T.pack $ show (lifeEventName x)))
        doLifeEvent x convertLE getA
  log Debug ("Doing " <> (T.pack $ show curDate) <> " life events.")
  Result _ results <- runResultT $ mapM_ f liveEvents
  doLifeEventResult results

doLifeEventResult :: ( IsFlow fl
                     , MonadState s m
                     , HasBalanceSheet s a
                     , HasCashFlows s fl) => LifeEventOutput a fl -> m ()
doLifeEventResult (LifeEventOutput newAccounts newFlows) = do
  mapM_ insertAccount newAccounts
  mapM_ addFlow newFlows

doTax :: ( EngineC a fl le ru rm
         , Loggable m
         , MonadError FMCException m
         , TaxDataAppC s m
         , ReadsFinEnv s rm
         , ReadsFinState s
         , ReadsTaxData s
         , ReadsAccumulators s
         , HasAccumulators s
         , HasBalanceSheet s a
         , ReadsMCState s a fl le ru
         , HasCashFlow s) => m (MoneyValue,Double)
doTax = do
  (taxPre, _) <- computeTax -- get amount
  addToAccumulator ("TaxOwed") taxPre  -- store amount for tax sweep rule.  Ick.
  doTaxTrade
  (tax', rate') <- computeTax -- recompute tax since trade may have incurred cap gains
  payTax tax'  -- pay tax, zero tax counters and carry forward losses
  zeroAccumulator ("TaxOwed")
  return (tax',rate')

doTaxTrade :: ( IsAsset a
              , Show a
              , IsRule ru
--              , IsRateModel rm
              , Loggable m
              , MonadError FMCException m
              , MonadState s m
              , ReadsFinState s
              , ReadsExchangeRateFunction s
              , ReadsFinEnv s rm
              , ReadsAccumulators s
              , HasTaxData s
              , HasCashFlow s
              , HasBalanceSheet s a
              , HasAccumulators s
              , ReadsMCState s a fl le ru) => m ()
doTaxTrade = do
  bs <- use balanceSheet
  curPos <- use cashFlow
  taxTrade <- use $ getMCState.mcsTaxTrade
  let getA name = getAccount name bs
  log Debug ("Current cash on hand is " <> (T.pack $ show curPos))
  log Debug "Generating tax trade, if necessary"
  Result _ result <- runResultT (doRule taxTrade getA)
  doRuleResult result

payTax :: ( MonadError FMCException m
          , Loggable m
          , TaxDataAppC s m
          , HasCashFlow s) => MoneyValue -> m ()
payTax tax = do
  let taxPmt = MV.negate tax
  addCashFlow taxPmt
  carryForwardTaxData
  log Debug ("Paid Tax of " <> (T.pack $ show tax))

doSweepTrades :: ( IsAsset a
                 , Show a
                 , IsRule ru
                 , Loggable m
                 , MonadError FMCException m
                 , MonadState s m
                 , HasTaxData s
                 , HasCashFlow s
                 , HasBalanceSheet s a
                 , HasAccumulators s
                 , ReadsAccumulators s
                 , ReadsFinState s
                 , ReadsFinEnv s rm
                 , ReadsExchangeRateFunction s
                 , ReadsMCState s a fl le ru) => m ()
doSweepTrades = do
  bs <- use balanceSheet
  sweepRule <- use $ getMCState.mcsSweep
  let getA name = getAccount name bs
  Result _ result <- runResultT (doRule sweepRule getA)
  doRuleResult result

doAccountTransaction :: ( IsAsset a
                        , Show a
                        , Loggable m
                        , MonadError FMCException m
                        , MonadState s m
                        , HasTaxData s
                        , HasCashFlow s
                        , ReadsExchangeRateFunction s
                        , HasBalanceSheet s a) => Transaction -> m ()
doAccountTransaction tr@(Transaction target typ amt)= do
  bs <- use balanceSheet
  Result after flows <- do
    acct <- getAccount target bs
    runResultT $ tradeAccount acct typ amt

  applyFlows flows
  insertAccount after

  cf <- use cashFlow
  log Debug ("Current net cash flow is " <> (T.pack $ show cf))
  log Debug ("Did Transaction (" <> (T.pack $ show tr) <> ")")
  log Debug ("With resulting account " <> (T.pack $ show after) <> " and flows " <> (T.pack $ show flows))


isNonZeroTransaction :: Transaction -> Bool
isNonZeroTransaction (Transaction _ _ (MoneyValue x _)) = x/=0

doTransactions :: ( IsAsset a
                  , Show a
                  , Loggable m
                  , MonadError FMCException m
                  , MonadState s m
                  , HasTaxData s
                  , ReadsExchangeRateFunction s
                  , HasCashFlow s
                  , HasBalanceSheet s a)
  => [Transaction]
  -> m ()
doTransactions ts = mapM_ doAccountTransaction (filter isNonZeroTransaction ts)

computeTax :: ( MonadError FMCException m
              , Loggable m
              , MonadState s m
              , ReadsFinEnv s rm
              , ReadsExchangeRateFunction s
              , ReadsTaxData s)
               => m (MoneyValue,Double)  -- main body now in Tax.hs
computeTax = do
  tr <- use $ getFinEnv.feTaxRules
  td <- use getTaxData
  (total,rate) <- fullTaxCV tr td
  log Debug ("Computed tax of "
             <> (T.pack $ show total)
             <> " (eff rate of "
             <> (T.pack $ show (100*rate))
             <> "%) given tax data: "
             <> (T.pack $ show td))
  return  (total,rate)

