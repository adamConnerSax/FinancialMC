{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module FinancialMC.Core.MCState
       (
         BalanceSheet(BalanceSheet)
       , HasBalanceSheet (..)
       , getAccountNames
       , makeNewBalanceSheet
       , insertAccount --NB: replaces if already present, like Map.insert
       , CashFlows (CashFlows)
       , HasCashFlows (..)
       , makeNewCashFlows
       , addFlow
       , ComponentTypes (..)
       , ShowableComponents
       , ToJSONComponents
       , FromJSONComponents
       , MCState(MCState)
       , HasMCState(..)
       , ReadsMCState (..)
       , HasRules
       , HasLifeEvents
       , makeMCState
       , getAccount
       , addRule
       , addLifeEvent
       , CombinedState(CombinedState)
       , HasCombinedState(..)
       , ReadsCombinedState (..)
       , evolveMCS
       , netWorth
       , netWorthBreakout
       , grossFlows
       , PathSummary(..)
       , isZeroNW
       , NetWorthArray
       , FSSummary (FSSummary)
       , HasFSSummary (..)
       , DatedSummary (DatedSummary)
       , HasDatedSummary (..)
       , summarize
       , computeFlows
       ) where


import           FinancialMC.Core.Asset (IsAsset, AccountName,Account(Account),HasAccount(..),accountValueCV)
import           FinancialMC.Core.Flow (IsFlow)
import           FinancialMC.Core.LifeEvent (IsLifeEvent)
import           FinancialMC.Core.Rule (IsRule)
import           FinancialMC.Core.Rates (IsRateModel)
import           FinancialMC.Core.Tax (HasTaxData)
import           FinancialMC.Core.Evolve (Evolvable(evolve),EvolveC, evolveWithin,evolveAndApply)
import           FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..), ReadsFinEnv (..) , FinState,HasFinState(..), HasAccumulators, HasCashFlow)
import           FinancialMC.Core.Flow (FlowName,FlowDirection(..),flowName,flowingAt,IsFlow(..),annualFlowAmount)
import           FinancialMC.Core.MapLike (IsMap(..))
import           FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),HasMoneyValue(..), ReadsExchangeRateFunction)
import qualified FinancialMC.Core.MoneyValueOps as MV
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.TradingTypes (LiquidityType(NearCash),liquidityType)
import           FinancialMC.Core.Utilities (noteM,FMCException(Other),Year)

import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Aeson hiding ((.=))
import           Data.Aeson.Types hiding ((.=))
import           GHC.Generics (Generic)

import           Control.DeepSeq (NFData(..))
import           Control.Lens (makeClassy,use,(^.),(%=),(.=),(<>=), Getter, Lens')
import Control.Monad.Except (MonadError)

import           Control.Monad (when)
import           Control.Monad.Reader (ask)
import           Control.Monad.State.Strict (State,MonadState,get)

type MyMap k v = M.Map k v
type AccountMap a = MyMap AccountName (Account a)
type FlowMap fl = MyMap FlowName fl

data BalanceSheet a = BalanceSheet { _bsAccountMap:: !(AccountMap a) } deriving (Generic)
makeClassy ''BalanceSheet

instance Functor BalanceSheet where
  fmap f (BalanceSheet am) = BalanceSheet (fmap f <$> am)

instance Evolvable a=>Evolvable (BalanceSheet a) where
  evolve bs  = evolveWithin bs bsAccountMap
  {-# INLINE evolve #-}

instance Show a=>Show (BalanceSheet a) where
  show (BalanceSheet as) = "Balance Sheet:" ++ mShow as

instance ToJSON a=>ToJSON (BalanceSheet a) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

instance FromJSON a=>FromJSON (BalanceSheet a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

getAccountNames :: BalanceSheet a -> [AccountName]
getAccountNames bs = mKeys (bs ^. bsAccountMap)

data CashFlows fl = CashFlows { _cfdFlowMap :: !(FlowMap fl) } deriving (Generic)
makeClassy ''CashFlows

instance Functor CashFlows where
  fmap f (CashFlows cfm) = CashFlows (f <$> cfm)

instance Evolvable fl=>Evolvable (CashFlows fl) where
  evolve cfd = evolveWithin cfd cfdFlowMap
  {-# INLINE evolve #-}

instance Show fl=>Show (CashFlows fl) where
  show (CashFlows ps) = "Payments:" ++ mShow ps

instance ToJSON fl=>ToJSON (CashFlows fl) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}

instance FromJSON fl=>FromJSON (CashFlows fl) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}

-- NB: These are unsafe adds and could overwrite if the names are the same

makeNewBalanceSheet :: BalanceSheet a
makeNewBalanceSheet = BalanceSheet mEmpty

makeNewCashFlows :: CashFlows fl
makeNewCashFlows = CashFlows mEmpty

insertAccount :: (IsAsset a, MonadState s m,  HasBalanceSheet s a) => Account a -> m ()
insertAccount acct@(Account name _ _ _) = balanceSheet.bsAccountMap %=  mInsert name acct
{-# INLINE insertAccount #-}

addFlow :: (IsFlow fl, MonadState s m, HasCashFlows s fl) => fl -> m ()
addFlow f = cashFlows.cfdFlowMap %= mInsert (flowName f) f
{-# INLINE addFlow #-}

getAccount :: MonadError FMCException m => AccountName -> BalanceSheet a -> m (Account a) --Either SomeException (Account a)
getAccount name (BalanceSheet am) = noteM (Other ("Failed to find account with name \"" <> (T.pack $ show name) <> "\"")) $ mLookup name am
{-# INLINE getAccount #-}

putAccount :: Account a -> AccountName -> BalanceSheet a -> BalanceSheet a
putAccount acct s (BalanceSheet am)  = BalanceSheet (mInsert s acct am)
{-# INLINE putAccount #-}

data PathSummary = FinalNW !MoneyValue | ZeroNW !Year

instance Show PathSummary where
  show (FinalNW mv) = "Final Net Worth: " ++ show mv
  show (ZeroNW day) = "Bankrupted in " ++ show day

instance NFData PathSummary where
  rnf (FinalNW x) = rnf x `seq` ()
  rnf (ZeroNW day) = rnf day `seq` ()

instance Eq PathSummary where
  (FinalNW x) == (FinalNW y) = x == y
  (ZeroNW d1) == (ZeroNW d2) = d1 == d2
  _ == _ = False
--  (ZeroNW _) == (FinalNW _) = False

instance Ord PathSummary where
  compare (FinalNW x) (FinalNW y) = compare (x ^. mAmount) (y ^. mAmount) -- NB: currency unsafe
  compare (ZeroNW d1) (ZeroNW d2) = compare d1 d2
  compare (FinalNW _) (ZeroNW _) = LT
  compare (ZeroNW _) (FinalNW _) = GT

isZeroNW::PathSummary->Bool
isZeroNW (FinalNW _) = False
isZeroNW (ZeroNW _) = True

type NetWorthArray = A.Array LiquidityType MoneyValue
data FSSummary = FSSummary { _fssNW:: !MoneyValue,
                             _fssNWBreakOut:: !NetWorthArray,
                             _fssInflow:: !MoneyValue,
                             _fssOutFlow:: !MoneyValue,
                             _fssTax:: !MoneyValue,
                             _fssTaxRate:: !Double }



instance Show FSSummary where
  show (FSSummary nw nwbo i o t tr) = "NW=" ++ show nw ++ " (" ++ show nwbo ++ "); in=" ++ show i
                                 ++ "; out=" ++ show o ++ "; tax=" ++ show t
                                 ++ "; tax rate=" ++ show (100*tr) ++ "%"



data DatedSummary = DatedSummary { _dsYear :: !Year, _dsSummary :: !FSSummary } deriving (Show)


class ( IsAsset (AssetType tag)
      , IsFlow (FlowType tag)
      , IsLifeEvent (LifeEventType tag)
      , IsRule (RuleType tag)
      , IsRateModel (RateModelType tag)) => ComponentTypes tag where
  type AssetType tag :: *
  type FlowType tag :: *
  type LifeEventType tag :: *
  type RuleType tag :: *
  type RateModelType tag :: *


type ShowableComponents tag = (ComponentTypes tag, Show (AssetType tag), Show (FlowType tag), Show (RuleType tag), Show (LifeEventType tag), Show (RateModelType tag))
type ToJSONComponents tag = (ComponentTypes tag, ToJSON (AssetType tag), ToJSON (FlowType tag), ToJSON (RuleType tag), ToJSON (LifeEventType tag), ToJSON (RateModelType tag))
type FromJSONComponents tag = (ComponentTypes tag, FromJSON (AssetType tag), FromJSON (FlowType tag), FromJSON (RuleType tag), FromJSON (LifeEventType tag), FromJSON (RateModelType tag))

-- GADT here so that the various XXXType are in scope and constrained in the constructor
data MCState tag where
  MCState :: ComponentTypes tag => { _mcsBalanceSheet :: !(BalanceSheet (AssetType tag))
                                   , _mcsCashFlows :: !(CashFlows (FlowType tag))
                                   , _mcsLifeEvents :: [LifeEventType tag]
                                   , _mcsRules :: [RuleType tag]
                                   , _mcsSweep :: RuleType tag
                                   , _mcsTaxTrade :: RuleType tag
                                   , _mcsPathSummary :: !PathSummary
                                   , _mcsHistory :: ![DatedSummary]
                                   } -> MCState tag

{-
data MCState a fl le ru = MCState { _mcsBalanceSheet:: !(BalanceSheet a)
                                  , _mcsCashFlows:: !(CashFlows fl)
                                  , _mcsLifeEvents:: [le]
                                  , _mcsRules:: [ru]
                                  , _mcsSweep:: ru
                                  , _mcsTaxTrade:: ru
                                  , _mcsPathSummary:: !PathSummary
                                  , _mcsHistory:: ![DatedSummary]
                                  }
-}

makeClassy ''FSSummary
makeClassy ''DatedSummary
makeClassy ''MCState

class ReadsMCState s tag | s -> tag where
  getMCState :: Getter s (MCState tag)
  default getMCState :: HasMCState s tag => Getter s (MCState tag)
  getMCState = mCState

instance (ComponentTypes tag, b ~ AssetType tag) => HasBalanceSheet (MCState tag) b where
  balanceSheet = mcsBalanceSheet

instance (ComponentTypes tag, b ~ FlowType tag) => HasCashFlows (MCState tag) b where
  cashFlows = mcsCashFlows

class HasRules s ru | s -> ru where
  rules :: Lens' s [ru]

instance (ComponentTypes tag, b ~ RuleType tag) => HasRules (MCState tag) b where
  rules = mcsRules

class HasLifeEvents s le | s -> le where
  lifeEvents :: Lens' s [le]

instance (ComponentTypes tag, b ~ LifeEventType tag) => HasLifeEvents (MCState tag) b where
  lifeEvents = mcsLifeEvents

instance ComponentTypes tag => Evolvable (MCState tag) where
  evolve (MCState bs cfd les rs sr ttr ps hist) = do
    newBS <- evolve bs
    newCFD <- evolve cfd
    let newMCS = MCState newBS newCFD les rs sr ttr ps hist
    return $ newMCS
  {-# INLINE evolve #-}

instance ShowableComponents tag => Show (MCState tag) where
  show (MCState bs cfd les rs sr ttr ps history) =
    show bs ++ "\n" ++ show cfd ++ "\n" ++
    "LifeEvents:\n " ++ foldl (\s e->s++ show e ++ "\n") "" les ++
    "Rules:\n " ++ foldl (\s r->s++ show r ++ "\n") "" rs ++
    "\nSweep Rule: " ++ show sr ++
    "\nTax Trade Rule:" ++ show ttr ++
    "\nSummary: " ++ show ps ++
    "\nHistory: " ++ show history


addRule :: (MonadState s m, HasRules s ru) => ru -> m ()
addRule r = rules %= flip (++) [r]
{-# INLINE addRule #-}

addLifeEvent :: (MonadState s m, HasLifeEvents s le) => le -> m ()
addLifeEvent le = lifeEvents %= flip (++) [le]
{-# INLINE addLifeEvent #-}

{-
-- Could be GADT here so that the constructor witnesses the constraint.  Is this necessary here?
data CombinedState tag where
  CombinedState :: ComponentTypes tag => { _csFinancial:: !FinState,
                                           _csMC:: !(MCState tag),
                                           _csNeedHistory:: !Bool } -> CombinedState tag
-}

data CombinedState tag = CombinedState { _csFinancial:: !FinState,
                                         _csMC:: !(MCState tag),
                                         _csNeedHistory:: !Bool }

{-
data CombinedState a fl le ru = CombinedState { _csFinancial:: !FinState,
                                                _csMC:: !(MCState a fl le ru),
                                                _csNeedHistory:: !Bool }
-}
makeClassy ''CombinedState

class ComponentTypes tag => ReadsCombinedState s tag | s -> tag where
  getCombinedState :: Getter s (CombinedState tag)
  default getCombinedState :: HasCombinedState s tag => Getter s (CombinedState tag)
  getCombinedState = combinedState

instance ComponentTypes tag => HasMCState (CombinedState tag) tag where
  mCState = csMC

instance ShowableComponents tag => Show (CombinedState tag) where
  show cs = "Financial:\n" ++ show (cs ^. csFinancial) ++ "\nMonteCarlo:\n" ++ show (cs ^. csMC)

netWorth :: ComponentTypes tag => CombinedState tag -> FinEnv rm -> MoneyValue
netWorth cs fe = CV.toMoneyValue ccy e $ foldr (\acct s -> s CV.|+| accountValueCV acct) initial' accts where
  e = fe ^. feExchange
  ccy = fe ^. feDefaultCCY
  BalanceSheet accts = cs ^. csMC.mcsBalanceSheet
  initial' = CV.fromMoneyValue $ cs ^. csFinancial.fsCashFlow

byLT :: LiquidityType -> Account a -> Bool
byLT lt acct = acctLT == lt where
  acctLT = liquidityType (acct ^. acType)

netWorthByLiquidityType :: ComponentTypes tag => CombinedState tag -> FinEnv rm -> LiquidityType -> MoneyValue
netWorthByLiquidityType cs fe lt = CV.toMoneyValue ccy e $ F.foldr (\acct s -> s CV.|+|  (value' acct)) initial' accts where
  e = fe ^. feExchange
  ccy = fe ^. feDefaultCCY
  z' =  CV.mvZero ccy
  BalanceSheet accts = cs ^. csMC.mcsBalanceSheet
  value' x = if byLT lt x then accountValueCV x else z'
  initial' = if lt == NearCash then (CV.fromMoneyValue $ cs ^. csFinancial.fsCashFlow) else z'


netWorthBreakout :: ComponentTypes tag => CombinedState tag -> FinEnv rm -> NetWorthArray
netWorthBreakout cs fe = A.listArray (minBound, maxBound) nws where
  lts = [(minBound::LiquidityType) ..]
  nws = fmap (netWorthByLiquidityType cs fe) lts


data FlowAccum' = FlowAccum' !CV.CVD !CV.CVD

grossFlows :: IsFlow fl =>  CashFlows fl -> FinEnv rm -> (MoneyValue, MoneyValue)
grossFlows (CashFlows flows) fe = (CV.toMoneyValue ccy e inF,CV.toMoneyValue ccy e outF) where
  e = fe ^. feExchange
  d = fe ^. feCurrentDate
  ccy = fe ^. feDefaultCCY
  z = CV.mvZero ccy
  amt' flow = if flowingAt d flow then (CV.fromMoneyValue $ annualFlowAmount flow) else z
  g flow (FlowAccum' x y) = case flowDirection flow of
    InFlow  -> FlowAccum' (x CV.|+| amt' flow) y
    OutFlow -> FlowAccum' x (y CV.|+| amt' flow)
  FlowAccum' inF outF = F.foldr g (FlowAccum' z z) flows


makeMCState :: ComponentTypes tag
  =>  BalanceSheet (AssetType tag)
  -> CashFlows (FlowType tag)
  -> FinEnv rm
  -> [LifeEventType tag]
  -> [RuleType tag]
  -> RuleType tag
  -> RuleType tag
  -> MCState tag
makeMCState bs cfd fe les rs sr ttr = MCState bs cfd les rs sr ttr (FinalNW z) [] where
  z = MV.zero  (fe ^. feDefaultCCY)

--NB: this is where all the evolution flows and accums finally get applied
-- ought to be able to replace the HasTaxData, HasCashFlow, HasAccumulators with HasFinState.  But how?
evolveMCS :: ( ComponentTypes tag
             , EvolveC s rm m
             , HasAccumulators s
             , HasCashFlow s
             , HasTaxData s
             , HasMCState s tag) => m ()
evolveMCS = do -- mCState %= evolveAndApply
  mcs <- use mCState
  mcs' <- evolveAndApply mcs
  mCState .= mcs'
{-# INLINE evolveMCS #-}

netWorth2PathSummary :: MoneyValue -> Year -> PathSummary
netWorth2PathSummary mv@(MoneyValue x _) y = ps where
  ps = if x>0 then FinalNW mv else ZeroNW y

addPathSummary :: PathSummary -> PathSummary -> PathSummary
addPathSummary (ZeroNW day) _ = ZeroNW day
addPathSummary (FinalNW _) (FinalNW y) = FinalNW y
addPathSummary (FinalNW _) (ZeroNW day) = ZeroNW day

-- TODO: More specific constraints
-- HasMCPathSummary
-- ReadsCombinedState
summarize :: ( ComponentTypes tag
             , MonadError FMCException m
             , MonadState s m
             , HasCombinedState s tag
             , ReadsFinEnv s rm)
  => MoneyValue
  -> MoneyValue
  -> MoneyValue
  -> Double
  -> m ()
summarize  inF outF tax taxRate = do
  cs <- use combinedState
  fe <- use getFinEnv
  let nw = netWorth cs fe
      endDate =  (fe ^. feCurrentDate) + 1 -- date is an integer year
      ps =  netWorth2PathSummary nw endDate
      prevPS =  cs ^. csMC.mcsPathSummary
      ps' =  ps `seq` addPathSummary prevPS ps
  combinedState.csMC.mcsPathSummary .=  ps'
  when (cs ^. csNeedHistory) $  addHistory inF outF tax taxRate
  return ()
{-# INLINE summarize #-}

computeFlows :: ( ComponentTypes tag
                , MonadError FMCException m
                , MonadState s m
                , ReadsMCState s tag
                , ReadsFinEnv s rm) => m  (MoneyValue, MoneyValue)
computeFlows = do
  cs <- use $ getMCState.mcsCashFlows
  fe <- use getFinEnv
  return $ grossFlows cs fe
{-# INLINE computeFlows #-}

-- TODO: More specific constraints
-- ReadsCombinedstate
-- HasMCHistory
addHistory :: ( ComponentTypes tag
              , MonadError FMCException m
              , MonadState s m
              , HasCombinedState s tag
              , ReadsFinEnv s rm)
  => MoneyValue
  -> MoneyValue
  -> MoneyValue
  -> Double
  -> m ()
addHistory inF outF tax effRate  = do
  cs <- use combinedState
  fe <- use getFinEnv
  let nw = netWorth cs fe
      nwbo = netWorthBreakout cs fe
      endDate = (fe ^. feCurrentDate) + 1
  combinedState.csMC.mcsHistory <>= [DatedSummary endDate (FSSummary nw nwbo inF outF tax effRate)]
{-# INLINE addHistory #-}
