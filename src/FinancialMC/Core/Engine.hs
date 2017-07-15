{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module FinancialMC.Core.Engine
       (
         RandomSeed
       , PathSummaryAndSeed (..)
       , HasPathSummaryAndSeed (..)
       , FMCPathState
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
import           FinancialMC.Core.Rates           (HasRateTable (rateTable),
                                                   IsRateModel, Rate,
                                                   ReadsRateTable)
import           FinancialMC.Core.Result          (Result (Result), ResultT,
                                                   runResultT)
import           FinancialMC.Core.Rule            (IsRule (..), RuleAppC,
                                                   RuleOutput (..),
                                                   RuleWhen (..))
import           FinancialMC.Core.TradingTypes    (Transaction (..))

import           FinancialMC.Core.FinApp          (HasPathState (stepEnv, stepState),
                                                   LogLevel (..),
                                                   Loggable (log),
                                                   PathState (PathState),
                                                   execPPathStack,
                                                   execPathStack, zoomPathState)

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
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction, HasExchangeRateFunction (exchangeRateFunction),
                                                   HasMoneyValue (..),
                                                   MoneyValue (MoneyValue),
                                                   ReadsExchangeRateFunction (getExchangeRateFunction))
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Rates           (InflationType (TaxBracket),
                                                   RateTag (Inflation),
                                                   applyModel, rateRequest)
import           FinancialMC.Core.Tax             (HasTaxData (taxData),
                                                   ReadsTaxData (getTaxData),
                                                   TaxData, TaxDataAppC,
                                                   carryForwardTaxData,
                                                   fullTaxCV, updateTaxRules)
import           FinancialMC.Core.Utilities       (AsFMCException (..),
                                                   FMCException (..), readOnly)

import           Prelude                          hiding (log)

import qualified Data.Text                        as T

import           Control.DeepSeq                  (deepseq)
import           Control.Lens                     (Lens', Zoom, Zoomed, magnify,
                                                   makeClassy, use, view, zoom,
                                                   (%=), (&), (+=), (.=), (.~),
                                                   (^.), _2)
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

type EngineC a fl le ru rm= ( IsLifeEvent le
                            , Show le
                            , IsAsset a
                            , Show a
                            , IsFlow fl
                            , Show fl
                            , IsRule ru
                            , Show ru
                            , IsRateModel rm)

type FullPathC s a fl le ru rm m = ( EngineC a fl le ru rm
                                   , Evolvable a
                                   , Evolvable fl
                                   , Loggable m
                                   , MonadError FMCException m
                                   , MonadState s m
                                   , ReadsFinState s
                                   , ReadsTaxData s
                                   , ReadsAccumulators s
                                   , HasFinEnv s rm
                                   , ReadsFinEnv s rm
                                   , ReadsExchangeRateFunction s
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

type FMCPathState a fl le ru rm = PathState (CombinedState a fl le ru) (FinEnv rm)

-- Make all the instances for PathState
instance HasTaxData (FMCPathState a fl le ru rm) where
  taxData = stepState . csFinancial . fsTaxData

instance ReadsTaxData (FMCPathState a fl le ru rm)

instance HasCashFlow (CombinedState a fl le ru) where
  cashFlow = csFinancial . fsCashFlow

instance HasCashFlow (FMCPathState a fl le ru rm) where
  cashFlow = stepState . csFinancial . fsCashFlow

instance  HasAccumulators (FMCPathState a fl le ru rm) where
  accumulators = stepState . csFinancial . fsAccumulators

instance ReadsAccumulators (FMCPathState a fl le ru rm)

instance HasFinState (FMCPathState a fl le ru rm) where
  finState = stepState . csFinancial

instance ReadsFinState (FMCPathState a fl le ru rm)

instance HasExchangeRateFunction (FinEnv rm) where
  exchangeRateFunction = feExchange

instance HasExchangeRateFunction (FMCPathState a fl le ru rm) where
  exchangeRateFunction = stepEnv . feExchange

instance ReadsExchangeRateFunction (FMCPathState a fl le ru rm)

instance HasRateTable (FMCPathState a fl le ru rm) Rate where
  rateTable = stepEnv . feRates

instance ReadsRateTable (FMCPathState a fl le ru rm) Rate

instance HasFinEnv (FMCPathState a fl le ru rm) rm where
  finEnv = stepEnv

instance ReadsFinEnv (FMCPathState a fl le ru rm) rm

instance HasBalanceSheet (FMCPathState a fl le ru rm) a where
  balanceSheet = stepState . csMC . mcsBalanceSheet

instance HasCashFlows (FMCPathState a fl le ru rm) fl where
  cashFlows = stepState . csMC . mcsCashFlows

instance HasMCState (FMCPathState a fl le ru rm) a fl le ru where
  mCState = stepState . csMC

instance ReadsMCState (FMCPathState a fl le ru rm) a fl le ru

instance HasCombinedState (FMCPathState a fl le ru rm) a fl le ru where
  combinedState = stepState


-- special ones for zooming
instance HasCashFlowExchangeRateFunction (FMCPathState a fl le ru rm) where
  cashFlowExchangeRateFunction = zoomPathState cashFlow exchangeRateFunction


-- The IO versions are required for logging.
-- If we only use IO for entropy, config loading and result reporting, we can use the pure stack.
-- But to do realtime logging to stdout we need IO in the stack all the time.

execOnePathIO :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> [LogLevel]
  -> FMCPathState a fl le ru rm
  -> RandomSeed
  -> Int
  -> IO (FMCPathState a fl le ru rm)
execOnePathIO convertLE logDetails ps0 seed years = do
  let newSource = pureMT seed
  execPPathStack (doPath convertLE newSource years) logDetails ps0

doPathsIO :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> [LogLevel]
  -> Bool
  -> FMCPathState a fl le ru rm
  -> Bool
  -> PureMT
  -> Int
  -> Int
  -> IO [PathSummaryAndSeed] --[(PathSummary,RandomSeed)]
doPathsIO convertLE logDetails showFinalStates ps0 singleThreaded pMT yearsPerPath paths = do
  let seeds = getNSeeds pMT paths
  let g seed = do
        PathState cs' _  <- execOnePathIO convertLE logDetails ps0 seed yearsPerPath
        when showFinalStates $ print cs'
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` (return $ PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
  if showFinalStates || singleThreaded || not (null logDetails) --parallelism doesn't play well with logging
    then mapM g seeds
    else CMP.mapM g seeds

execOnePathPure :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> FMCPathState a fl le ru rm
  -> RandomSeed
  -> Int
  -> Either FMCException (FMCPathState a fl le ru rm)
execOnePathPure convertLE ps0 seed years = do
  let newSource = pureMT seed
  execPathStack (doPath convertLE newSource years) ps0

doPaths :: EngineC a fl le ru rm
  => LifeEventConverters a fl le
  -> FMCPathState a fl le ru rm
  -> Bool
  -> PureMT
  -> Int
  -> Int
  -> Either FMCException [PathSummaryAndSeed]
doPaths convertLE ps0 singleThreaded pMT yearsPerPath paths = do
  let seeds =  getNSeeds pMT paths
      g seed = do
        PathState cs' _ <-  execOnePathPure convertLE ps0 seed yearsPerPath
        let q = cs' ^. csMC.mcsPathSummary
        q `seq` Right (PathSummaryAndSeed (cs' ^. csMC.mcsPathSummary) seed)
      eMap = seeds `deepseq` if singleThreaded then g <$> seeds
                             else g <$> seeds `using` parList rseq
  sequence eMap

doPath :: ( EngineC a fl le ru rm
          , Loggable m
          , MonadError FMCException m
          , CashFlowExchangeRateFunctionZoom s m0 m
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

doOneStepOnPath :: ( EngineC a fl le ru rm
                   , Loggable m
                   , MonadError FMCException m
                   , CashFlowExchangeRateFunctionZoom s m0 m
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
  fe <- use finEnv
--  model <- use $ finEnv.feRateModel
  (newModel,(newRates,newPMT)) <- applyModel (fe ^. feRates, pMT) (fe ^. feRateModel)
  finEnv %= (feRates .~ newRates) . (feRateModel .~ newModel)
--  finEnv.feRates .= newRates
--  finEnv.feRateModel .= newModel
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
             , CashFlowExchangeRateFunctionZoom s m0 m
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

class HasCashFlowExchangeRateFunction s where
  cashFlowExchangeRateFunction :: Lens' s (PathState MoneyValue ExchangeRateFunction)

type CashFlowExchangeRateFunctionZoom s m0 m = ( MonadError FMCException m0
                                               , Functor (Zoomed m0 ())
                                               , Zoom m0 m (PathState MoneyValue ExchangeRateFunction) s
                                               , HasCashFlowExchangeRateFunction s)

checkEndingCash' :: CashFlowExchangeRateFunctionZoom s m0 m => m ()
checkEndingCash' = zoom cashFlowExchangeRateFunction $ do
  PathState cash e <- get
  checkFalse (MV.gt e cash (MoneyValue 1 (cash ^. mCurrency))) ("Ending cash position " ++ show cash ++ " > 0")


doChecks :: ( MonadError FMCException m
            , CashFlowExchangeRateFunctionZoom s m0 m
            , MonadState s m
            , ReadsExchangeRateFunction s
            , ReadsFinState s) => m ()
doChecks = do
  checkEndingCash'

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
  curPos <- use cashFlow
--  taxTrade <- use $ getMCState.mcsTaxTrade
--  bs <- use balanceSheet
  mcs <- use getMCState
  let getA name = getAccount name (mcs ^. mcsBalanceSheet)
  log Debug ("Current cash on hand is " <> (T.pack $ show curPos))
  log Debug "Generating tax trade, if necessary"
  Result _ result <- runResultT (doRule (mcs ^. mcsTaxTrade) getA)
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
--  bs <- use balanceSheet
--  sweepRule <- use $ getMCState.mcsSweep
  mcs <- use getMCState
  let getA name = getAccount name (mcs ^. mcsBalanceSheet)
  Result _ result <- runResultT (doRule (mcs ^. mcsSweep) getA)
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
  acct <- getAccount target bs
  Result after flows <- runResultT $ tradeAccount acct typ amt
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
  (total, rate) <- fullTaxCV tr td
  log Debug ("Computed tax of "
             <> (T.pack $ show total)
             <> " (eff rate of "
             <> (T.pack $ show (100*rate))
             <> "%) given tax data: "
             <> (T.pack $ show td))
  return  (total,rate)

