{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module FinancialMC.Core.Rule
       (
         RuleName
       , RuleWhen(..)
       , IsRule(..)
       , showRuleCore
       , RuleOutput(..)
       , RuleAppC
       ) where

import           FinancialMC.Core.Asset           (AccountGetter, AccountName,
                                                   IsAsset)
import           FinancialMC.Core.Evolve          (AccumResult)
import           FinancialMC.Core.FinancialStates (ReadsAccumulators,
                                                   ReadsFinEnv, ReadsFinState)
import           FinancialMC.Core.MoneyValue      (ReadsExchangeRateFunction)
import           FinancialMC.Core.Rates           (IsRateModel)
import           FinancialMC.Core.Result          (MonadResult)
import           FinancialMC.Core.TradingTypes    (Transaction)
import           FinancialMC.Core.Utilities       (FMCException)

import           Control.Monad.Except             (MonadError)
import           Control.Monad.State              (MonadState)

import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

data RuleOutput = RuleOutput ![Transaction] ![AccumResult]
instance Semigroup RuleOutput where
  (RuleOutput x1 y1) <> (RuleOutput x2 y2) = RuleOutput (x1<>x2) (y1<>y2)

instance Monoid RuleOutput where
  mempty = RuleOutput [] []
  mappend = (<>)

--type RuleApp rm = ResultT RuleOutput (ReaderT FinState (ReaderT (FinEnv rm) (Either FMCException)))

type RuleAppC s rm m = ( MonadError FMCException m
                       , MonadResult RuleOutput m
                       , MonadState s m
                       , ReadsFinState s
                       , ReadsAccumulators s
                       , ReadsFinEnv s rm
                       , ReadsExchangeRateFunction s)

data RuleWhen = Special | BeforeTax | AfterSweep  deriving (Enum,Ord,Eq,Show,Read)

type RuleName = T.Text

class IsRule r where
  ruleName :: r -> RuleName
  ruleAccounts :: r -> [AccountName]
  ruleWhen :: r -> RuleWhen
  doRule :: (IsAsset a, {- IsRateModel rm,-} RuleAppC s rm m) => r -> AccountGetter m a -> m ()

showRuleCore :: IsRule r => r -> String
showRuleCore r = show (ruleName r) ++" (involves " ++ show (ruleAccounts r) ++ ")"


{-
data Rule where
  MkRule::(IsRule r,Show r,ToJSON r)=>r->Rule

instance Show Rule where
  show (MkRule r) = show r

instance TypeNamed Rule where
  typeName (MkRule r) = typeName r

instance IsRule Rule where
  ruleName (MkRule r) = ruleName r
  ruleAccounts (MkRule r) = ruleAccounts r
  doRule (MkRule r) = doRule r
  ruleWhen (MkRule r) = ruleWhen r

instance JSON_Existential Rule where
  containedName (MkRule r) = typeName r
  containedJSON (MkRule r) = toJSON r

instance ToJSON Rule where
  toJSON = existentialToJSON

instance HasParsers e Rule => EnvFromJSON e Rule where
  envParseJSON = parseJSON_Existential
-}
