{-# LANGUAGE TemplateHaskell,BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses #-}
module FinancialMC.Core.MoneyValue where

import Text.Printf (printf)
import Control.Lens (makeClassy)
import Control.DeepSeq (NFData(rnf))

import Data.List (intercalate)
import Data.List.Split (splitOn,chunksOf)

import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.Aeson.Types (genericToJSON,genericParseJSON,defaultOptions,Options(..))
import Data.Aeson.Existential.EnvParser (EnvFromJSON)
import GHC.Generics (Generic)


data Currency = USD | EUR deriving (Eq,Ord,Enum,Bounded,Show,Read,Generic,ToJSON,FromJSON)

instance EnvFromJSON e Currency


instance NFData Currency where
  rnf USD = ()
  rnf EUR = ()

type ExchangeRateFunction = Currency->Currency->Double
defaultExchangeRates::ExchangeRateFunction
defaultExchangeRates _ _ = 1.0

data MoneyValue = MoneyValue {_mAmount:: !Double, _mCurrency:: !Currency} deriving (Generic)
makeClassy ''MoneyValue

instance ToJSON MoneyValue where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2} 
 
instance FromJSON MoneyValue where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance Eq MoneyValue where
  MoneyValue a1 c1 == MoneyValue a2 c2 = (a1 == a2) && (c1 == c2)
  
instance NFData MoneyValue where
  rnf (MoneyValue x ccy) = rnf x `seq` rnf ccy `seq` ()


readsMoneyValue::ReadS MoneyValue 
readsMoneyValue s = [(MoneyValue x c,r2) | 
                     (xS,r) <- lex s, 
                     (x,_)<-reads xS, 
                     (cS,r2)<-lex r, 
                     (c,_)<-reads cS]  

instance Read MoneyValue where
  readsPrec _  = readsMoneyValue


instance Show MoneyValue where
  show (MoneyValue x ccy) = printf "%.2f" x ++ " " ++ show ccy


prettyPrintMV::MoneyValue->String
prettyPrintMV (MoneyValue x ccy) = result where
  ccy_symbol = "$" --this needs fixing
  as_string = printf "%.2f" x
  a = splitOn "." as_string
  whole = head a
  frac = a !! 1
  wps = intercalate "," $ map reverse $ reverse $ chunksOf 3 (reverse whole)
  result = ccy_symbol ++ wps ++ "." ++ frac  ++ " " ++ show ccy
  
prettyPrintMVK::MoneyValue->String
prettyPrintMVK (MoneyValue x ccy) =  result where
  as_string = printf "%.0f" (x/1000)
  wps = intercalate "," $ map reverse $ reverse $ chunksOf 3 (reverse as_string)
  result = wps ++ " " ++ show ccy
  
