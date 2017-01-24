{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FinancialMC.Core.TradingTypes where

import FinancialMC.Core.Result (Result,ResultT)
import FinancialMC.Core.MoneyValue (MoneyValue)
import qualified FinancialMC.Core.MoneyValueOps as MV
import FinancialMC.Core.MoneyValueOps (ER)
import FinancialMC.Core.Evolve (FlowResult)

import Control.Exception (SomeException)
import Control.Lens (makeClassy)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON,ToJSON)
import qualified Data.Text as T

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
                     Liability deriving (Eq,Ord,Show,Enum,Bounded)


liquidityType::AccountType->LiquidityType
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



type TradeApp = ResultT [FlowResult] (ER (Either SomeException))
type TradeFunction a = a->AccountType->TradeType->MoneyValue->TradeApp a  
type LiquidateFunction a = a->AccountType->TradeType->TradeApp a  



