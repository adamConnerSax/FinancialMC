{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module FinancialMC.Core.MoneyValue where

import           Control.DeepSeq  (NFData (rnf))
import           Control.Lens     (Getter, Lens', makeClassy)
import           Data.Char        (chr)
import qualified Data.Text        as T
import           Text.Printf      (printf)

import qualified Data.Array       as A
import           Data.List        (intercalate)
import           Data.List.Split  (chunksOf, splitOn)

import           Data.Aeson       (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Options (..), defaultOptions,
                                   genericParseJSON, genericToJSON)
import           GHC.Generics     (Generic)


data Currency = USD | EUR deriving (Eq,Ord,Enum,Bounded,A.Ix,Show,Read,Generic,ToJSON,FromJSON)

class HasCurrency s where
  currency :: Lens' s Currency

instance HasCurrency Currency where
  currency = id

class ReadsCurrency s where
  getCurrency :: Getter s Currency
  default getCurrency :: HasCurrency s => Getter s Currency
  getCurrency = currency

currencyToChar :: Currency -> Char
currencyToChar USD = '$'
currencyToChar EUR = chr 164 -- ?

instance NFData Currency where
  rnf USD = ()
  rnf EUR = ()

type ExchangeRateFunction = Currency -> Currency -> Double
defaultExchangeRates :: ExchangeRateFunction
defaultExchangeRates _ _ = 1.0

class HasExchangeRateFunction s where
  exchangeRateFunction :: Lens' s ExchangeRateFunction

instance HasExchangeRateFunction ExchangeRateFunction where
  exchangeRateFunction = id

class ReadsExchangeRateFunction s where
  getExchangeRateFunction :: Getter s ExchangeRateFunction
  default getExchangeRateFunction :: HasExchangeRateFunction s => Getter s ExchangeRateFunction
  getExchangeRateFunction = exchangeRateFunction

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

readsMoneyValue :: ReadS MoneyValue
readsMoneyValue s = [(MoneyValue x c,r2) |
                     (xS,r) <- lex s,
                     (x,_)<-reads xS,
                     (cS,r2)<-lex r,
                     (c,_)<-reads cS]

instance Read MoneyValue where
  readsPrec _  = readsMoneyValue


instance Show MoneyValue where
  show (MoneyValue x ccy) = printf "%.2f" x ++ " " ++ show ccy


prettyPrintMV :: MoneyValue -> String
prettyPrintMV (MoneyValue x ccy) = result where
  as_string = printf "%.2f" x
  a = splitOn "." as_string
  whole = head a
  frac = a !! 1
  wps = intercalate "," $ map reverse $ reverse $ chunksOf 3 (reverse whole)
  result = [currencyToChar ccy] ++ wps ++ "." ++ frac

prettyPrintMVText :: MoneyValue -> T.Text
prettyPrintMVText = T.pack . prettyPrintMV

prettyPrintMVK::MoneyValue->String
prettyPrintMVK (MoneyValue x ccy) =  result where
  as_string = printf "%.0f" (x/1000)
  wps = intercalate "," $ map reverse $ reverse $ chunksOf 3 (reverse as_string)
  result = wps ++ " " ++ show ccy

