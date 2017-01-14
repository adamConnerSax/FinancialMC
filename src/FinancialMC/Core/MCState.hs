{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell,BangPatterns, GADTs, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module FinancialMC.Core.MCState 
       (
         BalanceSheet(BalanceSheet),
         getAccountNames,
         makeNewBalanceSheet,
         insertAccount, --NB: replaces if already present, like Map.insert
         CashFlows,makeNewCashFlows,
         addFlow,
         MCState(MCState),HasMCState(..),
         makeMCState,
         getAccount,--putAccount,
         addRule,
         addLifeEvent,
         CombinedState(CombinedState),HasCombinedState(..),
         evolveMCS,
         netWorth,netWorthBreakout,grossFlows,
         PathSummary(..),isZeroNW,NetWorthMap,FSSummary(FSSummary),HasFSSummary(..),summarize,computeFlows
       ) where


import           FinancialMC.Core.LifeEvent (LifeEvent)
import           FinancialMC.Core.Asset (IsAsset,AccountName,Account(Account),HasAccount(..),accountValueCV)
import           FinancialMC.Core.Evolve (Evolvable(evolve),evolveWithin,evolveAndApply)
import           FinancialMC.Core.FinApp (LoggableStepApp,zoomStep)
import           FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..),FinState,HasFinState(..))
import           FinancialMC.Core.Flow (FlowName,Flow,FlowDirection(..),flowName,flowingAt,IsFlow(..),annualFlowAmount)
import           FinancialMC.Core.MapLike (IsMap(..))
import           FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),HasMoneyValue(..))
import qualified FinancialMC.Core.MoneyValueOps as MV
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.Rule (Rule)
import           FinancialMC.Core.TradingTypes (LiquidityType(NearCash),liquidityType)
import           FinancialMC.Core.Utilities (noteM,FMCException(Other),Year)

import qualified Data.Map as M
--import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F

import           Data.Aeson hiding ((.=))
import           Data.Aeson.Types hiding ((.=))
import           GHC.Generics (Generic)

import           Control.DeepSeq (NFData(..))
import           Control.Exception (SomeException)
import           Control.Lens (makeClassy,use,(^.),(%=),(.=),(<>=))

import           Control.Monad (when)
import           Control.Monad.Reader (ask)
import           Control.Monad.State.Strict (State,MonadState,get)

import           Data.Aeson.Existential.EnvParser (EnvFromJSON(..))
import           Data.Aeson.Existential.Generic (genericEnvParseJSON)

--import qualified Data.Text as T 

type MyMap k v = M.Map k v


type AccountMap a = MyMap AccountName (Account a)
type FlowMap = MyMap FlowName Flow

data BalanceSheet a = BalanceSheet {_bsAccountMap:: !(AccountMap a) } deriving (Generic)
makeClassy ''BalanceSheet

instance Evolvable a=>Evolvable (BalanceSheet a) where
  evolve bs  = evolveWithin bs bsAccountMap    
    
instance Show a=>Show (BalanceSheet a) where    
  show (BalanceSheet as) = "Balance Sheet:" ++ mShow as 

instance ToJSON a=>ToJSON (BalanceSheet a) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

instance EnvFromJSON e (AccountMap a)=> EnvFromJSON e (BalanceSheet a) where
  envParseJSON = genericEnvParseJSON defaultOptions {fieldLabelModifier = drop 3}

getAccountNames::BalanceSheet a->[AccountName]                           
getAccountNames bs = mKeys (bs ^. bsAccountMap)
                           
data CashFlows = CashFlows { _cfdFlowMap:: !FlowMap } deriving (Generic)
makeClassy ''CashFlows

instance Evolvable CashFlows where
  evolve cfd = evolveWithin cfd cfdFlowMap
    
instance Show CashFlows where    
  show (CashFlows ps) = "Payments:" ++ mShow ps
   
instance ToJSON CashFlows where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}

instance EnvFromJSON e FlowMap => EnvFromJSON e CashFlows where
  envParseJSON = genericEnvParseJSON defaultOptions {fieldLabelModifier = drop 4}

-- NB: These are unsafe adds and could overwrite if the names are the same 
    
makeNewBalanceSheet::BalanceSheet a
makeNewBalanceSheet = BalanceSheet mEmpty

makeNewCashFlows::CashFlows
makeNewCashFlows = CashFlows mEmpty
                      
insertAccount::MonadState (BalanceSheet a) m=>Account a->m ()
insertAccount acct@(Account name _ _ _) = bsAccountMap %=  mInsert name acct
  

addFlow::Flow->State CashFlows ()
addFlow f = cfdFlowMap %= mInsert (flowName f) f

getAccount::AccountName->BalanceSheet a->Either SomeException (Account a)
getAccount name (BalanceSheet am) = noteM (Other ("Failed to find account with name \"" ++ show name ++ "\"")) $ mLookup name am 

putAccount::Account a->AccountName->BalanceSheet a->BalanceSheet a
putAccount acct s (BalanceSheet am)  = BalanceSheet (mInsert s acct am) 

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

type NetWorthMap = M.Map LiquidityType MoneyValue
data FSSummary = FSSummary { _fssNW:: !MoneyValue,
                             _fssNWBreakOut:: !NetWorthMap,
                             _fssInflow:: !MoneyValue,
                             _fssOutFlow:: !MoneyValue,
                             _fssTax:: !MoneyValue,
                             _fssTaxRate:: !Double }
makeClassy ''FSSummary

instance Show FSSummary where
  show (FSSummary nw nwbo i o t tr) = "NW=" ++ show nw ++ " (" ++ show nwbo ++ "); in=" ++ show i 
                                 ++ "; out=" ++ show o ++ "; tax=" ++ show t 
                                 ++ "; tax rate=" ++ show (100*tr) ++ "%"  

data MCState a = MCState { _mcsBalanceSheet:: !(BalanceSheet a), _mcsCashFlows:: !CashFlows, 
                           _mcsLifeEvents:: ![LifeEvent a], _mcsRules:: ![Rule a], _mcsSweep:: !(Rule a), _mcsTaxTrade:: !(Rule a), 
                           _mcsPathSummary:: !PathSummary, _mcsNWHistory:: ![(Year,MoneyValue)], _mcsHistory:: ![(Year,FSSummary)]}


makeClassy ''MCState

instance Evolvable a=>Evolvable (MCState a) where
  evolve (MCState bs cfd les rs sr ttr ps nws hist) = do 
    newBS <- evolve bs
    newCFD <- evolve cfd
    let newMCS = MCState newBS newCFD les rs sr ttr ps nws hist 
    return $! newMCS 
      
instance Show a=>Show (MCState a) where
  show (MCState bs cfd les rs sr ttr ps nws history) = 
    show bs ++ "\n" ++ show cfd ++ "\n" ++ 
    "LifeEvents:\n " ++ foldl (\s e->s++ show e ++ "\n") "" les ++ 
    "Rules:\n " ++ foldl (\s r->s++ show r ++ "\n") "" rs ++ 
    "\nSweep Rule: " ++ show sr ++
    "\nTax Trade Rule:" ++ show ttr ++
    "\nSummary: " ++ show ps ++
    "\nNWHistory: " ++ show nws ++
    "\nHistory: " ++ show history
                                                         
    
addRule::Rule a->State (MCState a) ()
addRule r = do
  rs <- use mcsRules
  mcsRules .= rs++[r] -- NB this is append so do I need to think about ordering?
  
addLifeEvent::LifeEvent a->State (MCState a) ()
addLifeEvent le = do
  les <- use mcsLifeEvents
  mcsLifeEvents .= les++[le] -- NB this is append so do I need to think about ordering?


data CombinedState a = CombinedState { _csFinancial:: !FinState, _csMC:: !(MCState a), _csNeedHistory:: !Bool } 
makeClassy ''CombinedState


instance Show a=>Show (CombinedState a) where
  show cs = "Financial:\n" ++ show (cs ^. csFinancial) ++ "\nMonteCarlo:\n" ++ show (cs ^. csMC)


netWorth::IsAsset a=>CombinedState a->FinEnv->MoneyValue
netWorth cs fe = CV.toMoneyValue ccy e $ foldr (\acct s -> s CV.|+| accountValueCV acct) initial' accts where 
  e = fe ^. feExchange
  ccy = fe ^. feDefaultCCY
  BalanceSheet accts = cs ^. csMC.mcsBalanceSheet
  initial' = CV.fromMoneyValue $ cs ^. csFinancial.fsCashFlow


byLT::LiquidityType->Account a->Bool
byLT lt acct = acctLT == lt where 
  acctLT = liquidityType (acct ^. acType)

netWorthByLiquidityType::IsAsset a=>CombinedState a->FinEnv->LiquidityType->MoneyValue
netWorthByLiquidityType cs fe lt = CV.toMoneyValue ccy e $ F.foldr (\acct s -> s CV.|+|  (value' acct)) initial' accts where 
  e = fe ^. feExchange
  ccy = fe ^. feDefaultCCY
  z' =  CV.mvZero ccy
  BalanceSheet accts = cs ^. csMC.mcsBalanceSheet
  value' x = if byLT lt x then accountValueCV x else z'
  initial' = if lt == NearCash then (CV.fromMoneyValue $ cs ^. csFinancial.fsCashFlow) else z'


netWorthBreakout::IsAsset a=>CombinedState a->FinEnv->NetWorthMap
netWorthBreakout cs fe = M.fromList (zip lts nws) where
  lts = [(minBound::LiquidityType) ..]
  nws = fmap (netWorthByLiquidityType cs fe) lts


data FlowAccum' = FlowAccum' !CV.CVD !CV.CVD
grossFlows::CashFlows->FinEnv->(MoneyValue,MoneyValue)
grossFlows (CashFlows flows) fe = (CV.toMoneyValue ccy e inF,CV.toMoneyValue ccy e outF) where
  e = fe ^. feExchange
  d = fe ^. feCurrentDate
  ccy = fe ^. feDefaultCCY
  z = CV.mvZero ccy
--  CashFlows flows = cs ^. csMC.mcsCashFlows
  amt' flow = if flowingAt d flow then (CV.fromMoneyValue $ annualFlowAmount flow) else z
  g flow (FlowAccum' x y) = case flowDirection flow of
    InFlow  -> FlowAccum' (x CV.|+| amt' flow) y
    OutFlow -> FlowAccum' x (y CV.|+| amt' flow)
  FlowAccum' inF outF = F.foldr g (FlowAccum' z z) flows


makeMCState::BalanceSheet a->CashFlows->FinEnv->[LifeEvent a]->[Rule a]->Rule a->Rule a->MCState a
makeMCState bs cfd fe les rs sr ttr = MCState bs cfd les rs sr ttr (FinalNW z) [] [] where
  z = MV.zero  (fe ^. feDefaultCCY)

--NB: this is where all the evolution flows and accums finally get applied
evolveMCS::Evolvable a=>LoggableStepApp (CombinedState a) FinEnv app=>app ()
evolveMCS = do
  mcs <- use csMC
  mcs' <- zoomStep csFinancial $ evolveAndApply mcs
  csMC .= mcs'

--evolveMCS'::LoggableStepApp CombinedState app=>app ()
--evolveMCS' = csMC %= \x -> zoomStep csFinancial $  evolveAndApply x


netWorth2PathSummary::MoneyValue->Year->PathSummary
netWorth2PathSummary mv@(MoneyValue x _) y = ps where
  ps = if x>0 then FinalNW mv else ZeroNW y

addPathSummary::PathSummary->PathSummary->PathSummary
addPathSummary (ZeroNW day) _ = ZeroNW day
addPathSummary (FinalNW _) (FinalNW y) = FinalNW y
addPathSummary (FinalNW _) (ZeroNW day) = ZeroNW day 


summarize::IsAsset a=>LoggableStepApp (CombinedState a) FinEnv app=>MoneyValue->MoneyValue->MoneyValue->Double->app ()
summarize  inF outF tax taxRate = do
  cs <- get
  fe <- ask  
  needHistory <- use csNeedHistory
  let nw = netWorth cs fe
      endDate =  (fe ^. feCurrentDate) + 1 -- date is an integer year
      ps =  netWorth2PathSummary nw endDate
      prevPS =  cs ^. csMC.mcsPathSummary
      ps' =  ps `seq` addPathSummary prevPS ps
  csMC.mcsPathSummary .=  ps'
  when needHistory $  addHistory inF outF tax taxRate
  return ()

computeFlows::LoggableStepApp CashFlows FinEnv app=>app (MoneyValue,MoneyValue)
computeFlows = do
  cs <- get --use (csMC.mcsCashFlows)
  fe <- ask  
  return $ grossFlows cs fe

addHistory::IsAsset a=>LoggableStepApp (CombinedState a) FinEnv app=>MoneyValue->MoneyValue->MoneyValue->Double->app ()
addHistory inF outF tax effRate  = do
  cs <- get
  fe <- ask  
  let nw = netWorth cs fe
      nwbo = netWorthBreakout cs fe
      endDate = (fe ^. feCurrentDate) + 1
  csMC.mcsNWHistory <>= [(endDate,nw)]    
  csMC.mcsHistory <>= [(endDate,FSSummary nw nwbo inF outF tax effRate)]
  
