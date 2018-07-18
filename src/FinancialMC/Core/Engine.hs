{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module FinancialMC.Core.Engine
       (
         RandomSeed
       , PathSummaryAndSeed (..)
       , HasPathSummaryAndSeed (..)
       , FMCPathState (MkFMCPathState)
       , doOneYear
       , doPath
       , execOnePathIO
       , execOnePathPure
       , doPathsIO
       , doPaths
       , EngineC
         -- Only for Benchmarking
       , doChecks
       , computeTax
       , doTax
       ) where

import           FinancialMC.Core.Asset           (IsAsset)
import           FinancialMC.Core.AssetTrading    (tradeAccount)
import           FinancialMC.Core.Evolve          (Evolvable, applyAccums,
                                                   applyFlows)
import           FinancialMC.Core.FinancialStates (FinEnv, FinState, HasAccumulators (accumulators),
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
import           FinancialMC.Core.Result          (Result (Result), ResultT,
                                                   runResultT)
import           FinancialMC.Core.Rule            (IsRule (..), RuleAppC,
                                                   RuleOutput (..),
                                                   RuleWhen (..))
import           FinancialMC.Core.TradingTypes    (Transaction (..))

import           FinancialMC.Core.FinApp          (HasPathState (..),
                                                   LogLevel (..),
                                                   Loggable (log), PathStack,
                                                   PathStackable (..),
                                                   PathState (..),
                                                   pattern PathState,
                                                   ReadOnly (..),
                                                   execPPathStack,
                                                   execPathStack, stepEnv,
                                                   stepState, zoomPathState)

import           FinancialMC.Core.MCState         (CombinedState,
                                                   ComponentTypes (..),
                                                   HasBalanceSheet (..),
                                                   HasCashFlows (..),
                                                   HasCombinedState (..),
                                                   HasMCState (..),
                                                   PathSummary (..),
                                                   ReadsCombinedState (getCombinedState),
                                                   ReadsMCState (getMCState),
                                                   ShowableComponents (..),
                                                   addFlow, computeFlows,
                                                   evolveMCS, getAccount,
                                                   insertAccount, summarize)


import           FinancialMC.Core.LifeEvent       (IsLifeEvent (..),
                                                   LifeEventAppC,
                                                   LifeEventConverters,
                                                   LifeEventOutput (..),
                                                   lifeEventName, lifeEventYear)
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction, HasExchangeRateFunction (exchangeRateFunction),
                                                   HasMoneyValue (..),
                                                   MoneyValue (MoneyValue),
                                                   ReadsExchangeRateFunction (getExchangeRateFunction))
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Rates           (HasRateTable (rateTable),
                                                   InflationType (TaxBracket),
                                                   IsRateModel, RSource, Rate,
                                                   RateTag (Inflation),
                                                   ReadsRateTable, rateRequest,
                                                   runModel)
import           FinancialMC.Core.Tax             (HasTaxData (taxData),
                                                   ReadsTaxData (getTaxData),
                                                   TaxData, TaxDataAppC,
                                                   carryForwardTaxData,
                                                   fullTaxCV, fullTaxEDSL,
                                                   updateTaxRules)
import           FinancialMC.Core.Utilities       (AsFMCException (..),
                                                   FMCException (..), readOnly)

import           Prelude                          hiding (log)

import qualified Data.Text                        as T

import           Control.DeepSeq                  (deepseq)
import           Control.Lens                     (Lens', Zoom, Zoomed, lens,
                                                   magnify, makeClassy, use,
                                                   view, zoom, (%=), (&), (+=),
                                                   (.=), (.~), (^.), _2)
import           Control.Monad                    (foldM, unless, when)
import           Control.Monad.Morph              (hoist, lift)
import qualified Control.Monad.Parallel           as CMP
import           Control.Monad.Reader             (ReaderT, ask)
import           Control.Monad.State.Strict       (MonadState, get)
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

type EngineC tag = ShowableComponents tag

type FullPathC s tag m = ( EngineC tag
                         , Loggable m
                         , MonadError FMCException m
                         , MonadState s m
                         , ReadsFinState s
                         , ReadsTaxData s
                         , ReadsAccumulators s
                         , HasFinEnv s (RateModelType tag)
                         , ReadsFinEnv s (RateModelType tag)
                         , ReadsExchangeRateFunction s
                         , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
                         , HasAccumulators s
                         , HasCashFlow s
                         , HasBalanceSheet s (AssetType tag)
                         , HasCashFlows s (FlowType tag)
                         , HasTaxData s
                         , HasCombinedState s tag
                         , HasMCState s tag
                         , ReadsMCState s tag)

data FMCPathState tag where
  MkFMCPathState :: (ComponentTypes tag, rm ~ RateModelType tag) => PathState (CombinedState tag) (FinEnv rm) -> FMCPathState tag


instance (ComponentTypes tag, Show (PathState (CombinedState tag) (FinEnv (RateModelType tag)))) => Show (FMCPathState tag) where
  show (MkFMCPathState ps) = show ps

instance (ComponentTypes tag, rm ~ RateModelType tag) => HasPathState (FMCPathState tag) (CombinedState tag) (FinEnv rm) where
  pathState = lens (\(MkFMCPathState ps) -> ps) (\_ ps -> MkFMCPathState ps)
{-
instance ReadsPathState (FMCPathState tag) where
  getPathState = lens (\(MkFMCPathState ps) -> ps) (\_ ps -> MkFMCPathState ps)
-}
-- Make all the instances for PathState
instance (ComponentTypes tag) => HasTaxData (FMCPathState tag) where
  taxData = stepState . csFinancial . fsTaxData

instance (ComponentTypes tag) => ReadsTaxData (FMCPathState tag)

instance (ComponentTypes tag) => HasCashFlow (CombinedState tag) where
  cashFlow = csFinancial . fsCashFlow

instance (ComponentTypes tag) => HasCashFlow (FMCPathState tag) where
  cashFlow = stepState . csFinancial . fsCashFlow

instance  (ComponentTypes tag) => HasAccumulators (FMCPathState tag) where
  accumulators = stepState . csFinancial . fsAccumulators

instance (ComponentTypes tag) => ReadsAccumulators (FMCPathState tag)

instance (ComponentTypes tag) => HasFinState (FMCPathState tag) where
  finState = stepState . csFinancial

instance (ComponentTypes tag) => ReadsFinState (FMCPathState tag)

instance HasExchangeRateFunction (FinEnv rm) where
  exchangeRateFunction = feExchange

instance (ComponentTypes tag) => HasExchangeRateFunction (FMCPathState tag) where
  exchangeRateFunction = stepEnv . feExchange

instance (ComponentTypes tag) => ReadsExchangeRateFunction (FMCPathState tag)

instance (ComponentTypes tag) => HasRateTable (FMCPathState tag) Rate where
  rateTable = stepEnv . feRates

instance (ComponentTypes tag) => ReadsRateTable (FMCPathState tag) Rate

instance (ComponentTypes tag, rm ~ RateModelType tag) =>  HasFinEnv (FMCPathState tag) rm where
  finEnv = stepEnv

instance (ComponentTypes tag, rm ~ RateModelType tag) => ReadsFinEnv (FMCPathState tag) rm

instance (ComponentTypes tag, b ~ AssetType tag) => HasBalanceSheet (FMCPathState tag) b where
  balanceSheet = stepState . csMC . mcsBalanceSheet

instance (ComponentTypes tag, b ~ FlowType tag) => HasCashFlows (FMCPathState tag) b where
  cashFlows = stepState . csMC . mcsCashFlows

instance (ComponentTypes tag) => HasMCState (FMCPathState tag) tag where
  mCState = stepState . csMC

instance (ComponentTypes tag) => ReadsMCState (FMCPathState tag) tag

instance (ComponentTypes tag) => HasCombinedState (FMCPathState tag) tag where
  combinedState = stepState

-- special ones for zooming
--instance HasCashFlowExchangeRateFunction (FMCPathState a fl le ru rm) where
--  cashFlowExchangeRateFunction = zoomPathState cashFlow exchangeRateFunction


-- The IO versions are required for logging.
-- If we only use IO for entropy, config loading and result reporting, we can use the pure stack.
-- But to do realtime logging to stdout we need IO in the stack all the time.

execOnePathIO :: EngineC tag
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
  -> [LogLevel]
  -> FMCPathState tag
  -> RandomSeed
  -> Int
  -> IO (FMCPathState tag)
execOnePathIO convertLE logDetails ps0 seed years = do
  let newSource = pureMT seed
  execPPathStack (doPath convertLE newSource years) logDetails ps0

doPathsIO :: EngineC tag
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
  -> [LogLevel]
  -> Bool
  -> FMCPathState tag
  -> Bool
  -> PureMT
  -> Int
  -> Int
  -> IO [PathSummaryAndSeed] --[(PathSummary,RandomSeed)]
doPathsIO convertLE logDetails showFinalStates ps0 singleThreaded pMT yearsPerPath paths = do
  let seeds = getNSeeds pMT paths
  let g seed = do
        MkFMCPathState (PathState cs' _)  <- execOnePathIO convertLE logDetails ps0 seed yearsPerPath
        when showFinalStates $ print cs'
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` (return $ PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
  if showFinalStates || singleThreaded || not (null logDetails) --parallelism doesn't play well with logging
    then mapM g seeds
    else CMP.mapM g seeds

execOnePathPure :: EngineC tag
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
  -> FMCPathState tag
  -> RandomSeed
  -> Int
  -> Either FMCException (FMCPathState tag)
execOnePathPure convertLE ps0 seed years = do
  let newSource = pureMT seed
  execPathStack (doPath convertLE newSource years) ps0
{-# INLINEABLE execOnePathPure #-}

doPaths :: EngineC tag
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
  -> FMCPathState tag
  -> Bool
  -> PureMT
  -> Int
  -> Int
  -> Either FMCException [PathSummaryAndSeed]
doPaths convertLE ps0 singleThreaded pMT yearsPerPath paths = do
  let seeds =  getNSeeds pMT paths
      g seed = do
        MkFMCPathState (PathState cs' _) <-  execOnePathPure convertLE ps0 seed yearsPerPath
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` Right (PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
      eMap = seeds `deepseq` if singleThreaded then g <$> seeds
                             else g <$> seeds `using` parList rseq
  sequence eMap
{-# INLINEABLE doPaths #-}

doPath :: ( EngineC tag
          , Loggable m
          , MonadError FMCException m
          , MonadState s m
          , ReadsFinState s
          , ReadsTaxData s
          , ReadsAccumulators s
          , HasFinEnv s (RateModelType tag)
          , ReadsFinEnv s (RateModelType tag)
          , ReadsExchangeRateFunction s
          , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
          , HasAccumulators s
          , HasCashFlow s
          , HasBalanceSheet s (AssetType tag)
          , HasCashFlows s (FlowType tag)
          , HasTaxData s
          , HasCombinedState s tag
          , HasMCState s tag
          , ReadsMCState s tag)
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
  -> PureMT
  -> Int
  -> m PureMT
doPath convertLE pMT years = foldM (\a _ -> doOneStepOnPath convertLE a) pMT [1..years]
{-# INLINEABLE doPath #-}

doOneStepOnPath :: ( EngineC tag
                   , Loggable m
                   , MonadError FMCException m
                   , MonadState s m
                   , ReadsFinState s
                   , ReadsTaxData s
                   , ReadsAccumulators s
                   , HasFinEnv s (RateModelType tag)
                   , ReadsFinEnv s (RateModelType tag)
                   , ReadsExchangeRateFunction s
                   , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
                   , HasAccumulators s
                   , HasCashFlow s
                   , HasBalanceSheet s (AssetType tag)
                   , HasCashFlows s (FlowType tag)
                   , HasTaxData s
                   , HasCombinedState s tag
                   , HasMCState s tag
                   , ReadsMCState s tag)
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
  -> PureMT
  -> m PureMT
doOneStepOnPath convertLE pMT = do
  newPMT <- do
    x <- updateRates pMT -- order matters here.  We need to update rates to get the updated exchange rate.
    updateExchangeRateFunction
    return x
  (inFlow, outFlow) <- computeFlows
  (tax, effRate) <- doOneYear convertLE
  summarize inFlow outFlow tax effRate
  updateCurrentDate
  updateTaxBrackets --fix??
  return $! newPMT
{-# INLINEABLE doOneStepOnPath #-}

updateCurrentDate :: (MonadState s m, HasFinEnv s rm) => m ()
updateCurrentDate = (finEnv.feCurrentDate) += 1
{-# INLINEABLE updateCurrentDate #-}

updateRates :: ( IsRateModel rm
               , MonadError FMCException m
               , MonadState s m
               , HasFinEnv s rm) => PureMT -> m PureMT
updateRates pMT = do
  fe <- use finEnv
--  model <- use $ finEnv.feRateModel
  let ((newModel, newRates), newPMT) = runModel (fe ^. feRates) (fe ^. feRateModel) pMT
  finEnv.feRates .= newRates
  finEnv.feRateModel .= newModel
  return $! newPMT
{-# INLINEABLE updateRates #-}

updateTaxBrackets :: ( MonadError FMCException m
                     , MonadState s m
                     , ReadsRateTable s Double
                     , HasFinEnv s rm) => m ()
updateTaxBrackets = do
  taxBracketInflationRate <- rateRequest (Inflation TaxBracket)
  (finEnv.feTaxRules) %= flip updateTaxRules taxBracketInflationRate
{-# INLINEABLE updateTaxBrackets #-}

doOneYear :: ( EngineC tag
             , Loggable m
             , MonadError FMCException m
             , MonadState s m
             , ReadsFinState s
             , ReadsTaxData s
             , ReadsAccumulators s
             , ReadsFinEnv s (RateModelType tag)
             , ReadsExchangeRateFunction s
             , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
             , HasAccumulators s
             , HasCashFlow s
             , HasBalanceSheet s (AssetType tag)
             , HasCashFlows s (FlowType tag)
             , HasTaxData s
             , HasMCState s tag
             , ReadsMCState s tag)
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag)
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
{-# INLINEABLE doOneYear #-}

checkTrue :: MonadError FMCException m => Bool -> String -> m ()
checkTrue cond errMsg = unless cond $ throwing _Other (T.pack errMsg)
{-# INLINEABLE checkTrue #-}

checkFalse :: MonadError FMCException m => Bool -> String -> m ()
checkFalse cond errMsg = when cond $ throwing _Other (T.pack errMsg)
{-# INLINEABLE checkFalse #-}

checkEndingCash :: ( MonadError FMCException m
                   , MonadState s m
                   , ReadsExchangeRateFunction s
                   , ReadsFinState s) => m ()
checkEndingCash = do
  e <- use getExchangeRateFunction
  cash <- use $ getFinState.fsCashFlow
  checkFalse (MV.gt e cash (MoneyValue 1 (cash ^. mCurrency))) ("Ending cash position " ++ show cash ++ " > 0")
{-# INLINEABLE checkEndingCash #-}

{-
class HasCashFlowExchangeRateFunction s where
  cashFlowExchangeRateFunction :: Lens' s (PathState MoneyValue ExchangeRateFunction)

type CashFlowExchangeRateFunctionZoom s m0 m = ( MonadError FMCException m0
                                               , Functor (Zoomed m0 ())
                                               , Zoom m0 m (PathState MoneyValue ExchangeRateFunction) s
                                               , ReadOnly (PathState MoneyValue ExchangeRateFunction) m0
                                               , MonadError FMCException (ReaderM (PathState MoneyValue ExchangeRateFunction) m0)
                                               , HasCashFlowExchangeRateFunction s)

checkEndingCash' :: CashFlowExchangeRateFunctionZoom s m0 m => m ()
checkEndingCash' = zoom cashFlowExchangeRateFunction $ do
  PathState cash e <- get
  checkFalse (MV.gt e cash (MoneyValue 1 (cash ^. mCurrency))) ("Ending cash position " ++ show cash ++ " > 0")
{-# INLINEABLE checkEndingCash' #-}
-}

doChecks :: ( MonadError FMCException m
            , MonadState s m
            , ReadsExchangeRateFunction s
            , ReadsFinState s) => m ()
doChecks = do
  checkEndingCash
{-# INLINEABLE doChecks #-}

doRules :: ( Loggable m
           , EngineC tag
           , MonadError FMCException m
           , MonadState s m
           , ReadsFinState s
           , ReadsAccumulators s
           , ReadsFinEnv s (RateModelType tag)
           , ReadsExchangeRateFunction s
           , HasAccumulators s
           , HasTaxData s
           , HasCashFlow s
           , HasBalanceSheet s (AssetType tag)
           , ReadsMCState s tag)
  => RuleWhen -> m ()
doRules w = do
  mcs <- use getMCState
--  rs <- use $ getMCState.mcsRules
--  bs <- use $ getMCState.mcsBalanceSheet
  let isRuleNow r = ruleWhen r == w
      liveRules = filter isRuleNow (mcs ^. mcsRules)
      getA name = getAccount name (mcs ^. mcsBalanceSheet)
      f r = do
        log Debug ("Doing " <> (T.pack $ show (ruleName r)))
        doRule r getA
  log Debug ("Doing " <> (T.pack $ show w) <> " rules.")
  Result _ ruleOutput <- runResultT $ mapM_ f liveRules -- can/should move zoom/hoist to here?  Does it matter?
  doRuleResult ruleOutput
{-# INLINEABLE doRules #-}

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
{-# INLINEABLE doRuleResult #-}

doLifeEvents :: ( EngineC tag
                , Loggable m
                , MonadError FMCException m
                , MonadState s m
                , HasBalanceSheet s (AssetType tag)
                , HasCashFlows s (FlowType tag)
                , ReadsFinState s
                , ReadsFinEnv s (RateModelType tag)
                , ReadsExchangeRateFunction s
                , ReadsMCState s tag)
  => LifeEventConverters (AssetType tag) (FlowType tag) (LifeEventType tag) -> m ()
doLifeEvents convertLE = do
  curDate <- use $ getFinEnv.feCurrentDate
  mcs <- use getMCState
--  les <- use $ getMCState.mcsLifeEvents
--  bs <- use $ getMCState.mcsBalanceSheet
  let happeningThisYear le = (lifeEventYear le == curDate)
      liveEvents = filter happeningThisYear (mcs ^. mcsLifeEvents)
      getA name = getAccount name (mcs ^. mcsBalanceSheet)
      f x = do
        log Debug ("Doing " <> (T.pack $ show (lifeEventName x)))
        doLifeEvent x convertLE getA
  log Debug ("Doing " <> (T.pack $ show curDate) <> " life events.")
  Result _ results <- runResultT $ mapM_ f liveEvents
  doLifeEventResult results
{-# INLINEABLE doLifeEvents #-}

doLifeEventResult :: ( IsFlow fl
                     , IsAsset a
                     , MonadState s m
                     , HasBalanceSheet s a
                     , HasCashFlows s fl) => LifeEventOutput a fl -> m ()
doLifeEventResult (LifeEventOutput newAccounts newFlows) = do
  mapM_ insertAccount newAccounts
  mapM_ addFlow newFlows
{-# INLINEABLE doLifeEventResult #-}

doTax :: ( EngineC tag
         , Loggable m
         , MonadError FMCException m
         , TaxDataAppC s m
         , ReadsFinEnv s (RateModelType tag)
         , ReadsFinState s
         , ReadsTaxData s
         , ReadsAccumulators s
         , HasAccumulators s
         , HasBalanceSheet s (AssetType tag)
         , ReadsMCState s tag
         , HasCashFlow s) => m (MoneyValue,Double)
doTax = do
  (taxPre, _) <- computeTax -- get amount
  addToAccumulator ("TaxOwed") taxPre  -- store amount for tax sweep rule.  Ick.
  doTaxTrade
  (tax', rate') <- computeTax -- recompute tax since trade may have incurred cap gains
  payTax tax'  -- pay tax, zero tax counters and carry forward losses
  zeroAccumulator ("TaxOwed")
  return (tax',rate')
{-# INLINEABLE doTax #-}

doTaxTrade :: ( ShowableComponents tag
              , Loggable m
              , MonadError FMCException m
              , MonadState s m
              , ReadsFinState s
              , ReadsExchangeRateFunction s
              , ReadsFinEnv s (RateModelType tag)
              , ReadsAccumulators s
              , HasTaxData s
              , HasCashFlow s
              , HasBalanceSheet s (AssetType tag)
              , HasAccumulators s
              , ReadsMCState s tag) => m ()
doTaxTrade = do
  curPos <- use cashFlow
--  taxTrade <- use $ getMCState.mcsTaxTrade
--  bs <- use balanceSheet
  mcs <- use getMCState
  let getA name = getAccount name (mcs ^. mcsBalanceSheet)
  log Debug ("Current cash on hand is " <> (T.pack $ show curPos))
  log Debug "Generating tax trade, if necessary"
  Result _ result <- runResultT (doRule (mcs ^. mcsTaxTrade) getA)
  doRuleResult result
{-# INLINEABLE doTaxTrade #-}

payTax :: ( MonadError FMCException m
          , Loggable m
          , TaxDataAppC s m
          , HasCashFlow s) => MoneyValue -> m ()
payTax tax = do
  let taxPmt = MV.negate tax
  addCashFlow taxPmt
  carryForwardTaxData
  log Debug ("Paid Tax of " <> (T.pack $ show tax))
{-# INLINEABLE payTax #-}

doSweepTrades :: ( ShowableComponents tag
                 , Loggable m
                 , MonadError FMCException m
                 , MonadState s m
                 , HasTaxData s
                 , HasCashFlow s
                 , HasBalanceSheet s (AssetType tag)
                 , HasAccumulators s
                 , ReadsAccumulators s
                 , ReadsFinState s
                 , ReadsFinEnv s (RateModelType tag)
                 , ReadsExchangeRateFunction s
                 , ReadsMCState s tag) => m ()
doSweepTrades = do
--  bs <- use balanceSheet
--  sweepRule <- use $ getMCState.mcsSweep
  mcs <- use getMCState
  let getA name = getAccount name (mcs ^. mcsBalanceSheet)
  Result _ result <- runResultT (doRule (mcs ^. mcsSweep) getA)
  doRuleResult result
{-# INLINEABLE doSweepTrades #-}

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
  acct <- getAccount target bs
  Result after flows <- runResultT $ tradeAccount acct typ amt
  applyFlows flows
  insertAccount after

  cf <- use cashFlow
  log Debug ("Current net cash flow is " <> (T.pack $ show cf))
  log Debug ("Did Transaction (" <> (T.pack $ show tr) <> ")")
  log Debug ("With resulting account " <> (T.pack $ show after) <> " and flows " <> (T.pack $ show flows))
{-# INLINEABLE doAccountTransaction #-}

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
{-# INLINEABLE doTransactions #-}

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
  (total, rate) <- fullTaxEDSL tr td
  log Debug ("Computed tax of "
             <> (T.pack $ show total)
             <> " (eff rate of "
             <> (T.pack $ show (100*rate))
             <> "%) given tax data: "
             <> (T.pack $ show td))
  return  (total,rate)
{-# INLINEABLE computeTax #-}
