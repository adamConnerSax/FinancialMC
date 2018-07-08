{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module FinancialMC.Core.TradingTypes where

import           FinancialMC.Core.Evolve        (FlowResult)
import           FinancialMC.Core.MoneyValue    (MoneyValue,
                                                 ReadsExchangeRateFunction)
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Result        (MonadResult, Result)
import           FinancialMC.Core.Utilities     (FMCException)
--import Control.Exception (SomeException)

import           Control.Lens                   (makeClassy)

import           Control.Monad.State            (MonadState)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Text                      as T
import           GHC.Generics                   (Generic)
import Data.Array (Ix)

type TradeResult a = Result [FlowResult] a
data TradeType = NormalTrade | OverFund | EarlyWithdrawal deriving (Eq,Show,Read,Generic,ToJSON,FromJSON)

data AccountType = Bank
                 | Brokerage
                 | PrimaryHome
                 | A401k
                 | A529
                 | IRA
                 | RothIRA
                 | Debt
                 | OtherProperty
                 deriving (Eq,Show,Read,Enum,Bounded,Generic,FromJSON,ToJSON)



data LiquidityType = NearCash |
                     DeferredIncome |
                     RetirementSavings |
                     EducationSavings |
                     Property |
                     Liability deriving (Eq,Ord,Show,Enum,Bounded,Ix)


liquidityType :: AccountType->LiquidityType
liquidityType aType
  | aType `elem` [Bank, Brokerage] = NearCash
  | aType `elem` [A401k, IRA, RothIRA] = RetirementSavings
  | aType `elem` [A529]  = EducationSavings
  | aType `elem` [Debt] = Liability
  | aType `elem` [PrimaryHome,OtherProperty] = Property
  | otherwise = Property


data Transaction = Transaction {_tTarget:: !T.Text, _tType:: !TradeType, _tAmount:: !MoneyValue }
makeClassy ''Transaction

instance Show Transaction where
  show (Transaction tgt t amt)
     | MV.isPositive amt = show amt ++ " to " ++ show tgt ++ " (type=" ++ show t ++ ")"
     | otherwise = show (MV.negate amt) ++ " from " ++ show tgt ++ " (type=" ++ show t ++ ")"



type TradeAppC s m = (MonadResult [FlowResult] m, MonadState s m, ReadsExchangeRateFunction s) --ResultT [FlowResult] (ER (Either FMCException))
type TradeFunction s m a = TradeAppC s m => a -> AccountType -> TradeType -> MoneyValue -> m a --TradeApp a
type LiquidateFunction s m a = TradeAppC s m => a -> AccountType -> TradeType -> m a --TradeApp a



