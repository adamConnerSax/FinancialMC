{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module FinancialMC.Core.Rule
       (
         RuleName
       , RuleWhen(..)
       , IsRule(..)
       , showRuleCore
       , RuleOutput(..)
       , RuleApp
       ) where

import           FinancialMC.Core.Asset           (AccountGetter, AccountName,
                                                   IsAsset)
import           FinancialMC.Core.Evolve          (AccumResult)
import           FinancialMC.Core.FinancialStates (FinEnv, FinState)
import           FinancialMC.Core.Rates           (IsRateModel)
import           FinancialMC.Core.Result          (ResultT)
import           FinancialMC.Core.TradingTypes    (Transaction)
import           FinancialMC.Core.Utilities       (FMCException)

import           Control.Monad.Reader             (ReaderT)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

data RuleOutput = RuleOutput ![Transaction] ![AccumResult]
instance Monoid RuleOutput where
  mempty = RuleOutput [] []
  mappend (RuleOutput x1 y1) (RuleOutput x2 y2) = RuleOutput (x1<>x2) (y1<>y2)

type RuleApp rm = ResultT RuleOutput (ReaderT FinState (ReaderT (FinEnv rm) (Either FMCException)))

data RuleWhen = Special | BeforeTax | AfterSweep  deriving (Enum,Ord,Eq,Show,Read)

type RuleName = T.Text

class IsRule r where
  ruleName :: r -> RuleName
  ruleAccounts :: r -> [AccountName]
  ruleWhen :: r -> RuleWhen
  doRule :: (IsAsset a, IsRateModel rm) => r -> AccountGetter a -> RuleApp rm ()

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


