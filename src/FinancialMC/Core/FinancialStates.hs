{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module FinancialMC.Core.FinancialStates
       (
         FinEnv(FinEnv)
       , HasFinEnv (..)
       , ReadsFinEnv (getFinEnv)
       , FinState(FinState)
       , HasFinState(..)
       , ReadsFinState (getFinState)
       , HasTaxData (taxData)
       , HasCashFlow (cashFlow)
       , HasAccumulators (accumulators)
       , zeroFinState
       , currentDate
       , changeRate
       , changeCurrentDate
       , exchangeRFFromRateTable
       , updateExchangeRateFunction
       , AccumName
       , Accumulators(Accumulators)
       , addToAccumulator
       , addToAccumulator'
       , getAccumulatedValue
       , zeroAccumulator
       , addCashFlow
       ) where

import           FinancialMC.Core.MoneyValue    (Currency, ExchangeRateFunction,
                                                 MoneyValue,
                                                 ReadsExchangeRateFunction (..))
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Rates         (Rate, RateTable (..),
                                                 RateTag (Exchange))
import           FinancialMC.Core.Tax           (HasTaxData (..), TaxData,
                                                 TaxRules, defaultTaxData)
import           FinancialMC.Core.Utilities     (AsFMCException (..), FMCException (FailedLookup, Other),
                                                 Year, noteM)

import           Control.Exception.Lens         (exception, throwingM)
import           Control.Lens                   (Getter, Lens', makeClassy, use,
                                                 view, (%=), (.=), (^.))
import           Control.Monad                  (when)
--import           Control.Monad.Catch            (MonadThrow, throwM)
import           Control.Monad.Error.Lens       (throwing)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.Reader           (MonadReader, Reader, ask,
                                                 reader)
import           Control.Monad.State.Strict     (MonadState, State)
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromJust)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T

data FinEnv rm = FinEnv { _feRates::RateTable Double, _feExchange:: !ExchangeRateFunction , _feCurrentDate:: !Year, _feDefaultCCY:: !Currency, _feTaxRules:: !TaxRules, _feRateModel:: !rm}
makeClassy ''FinEnv

class ReadsFinEnv s rm | s -> rm where
  getFinEnv :: Getter s (FinEnv rm)
  default getFinEnv :: HasFinEnv s rm => Getter s (FinEnv rm)
  getFinEnv = finEnv

instance Show rm => Show (FinEnv rm) where
  show (FinEnv rates _ cd ccy tr rm) = "rates: " ++ show rates ++ "\nmodel: " ++ show rm ++ "\ndate: " ++ show cd ++ "\ncurrency: " ++ show ccy ++ "\ntax: " ++ show tr

currentDate::Reader (FinEnv rm) Year
currentDate = reader $ \e->e ^. feCurrentDate

-- functions to change external state between evolves

changeRate::RateTag->Double->State (FinEnv rm) ()
changeRate rateTag x = do
  rateTable <- use feRates
  feRates .= rSet rateTable rateTag x

changeCurrentDate::Year->State (FinEnv rm) ()
changeCurrentDate d = feCurrentDate .= d

exchangeRFFromRateTable::RateTable Rate->ExchangeRateFunction
exchangeRFFromRateTable rateTable ca cb =
  let ccys = [(minBound::Currency)..]
      pairs = [(x,y) | x<-ccys,y<-ccys]
      eRate (c1,c2) = fromJust (rLookup rateTable (Exchange c2))/fromJust (rLookup rateTable (Exchange c1))
      eRates = foldl (\m k->M.insert k (eRate k) m) M.empty pairs
  in fromJust (M.lookup (ca,cb) eRates)

updateExchangeRateFunction::MonadState (FinEnv rm) m=>m ()
updateExchangeRateFunction = do
  rateTable <- use feRates
  feExchange .= exchangeRFFromRateTable rateTable

-- state for accumulating tax data, cashflows, transactions, etc.  Changes during evolve
type AccumName = T.Text

newtype Accumulators = Accumulators { _accums:: M.Map AccumName MoneyValue }
makeClassy ''Accumulators

newAccumulators::Accumulators
newAccumulators = Accumulators M.empty

instance Show Accumulators where
  show (Accumulators as) = "Accumulators: \n" ++ M.foldrWithKey(\k a s -> s ++ "\n\t" ++ show k ++ ": " ++ show a) "" as

data FinState = FinState { _fsTaxData:: !TaxData, _fsCashFlow:: !MoneyValue, _fsAccumulators:: !Accumulators}
makeClassy ''FinState

class ReadsFinState s where
  getFinState :: Getter s FinState
  default getFinState :: HasFinState s => Getter s FinState
  getFinState = finState

class HasCashFlow s where
  cashFlow :: Lens' s MoneyValue

instance HasTaxData FinState where
  taxData = fsTaxData

instance HasCashFlow FinState where
  cashFlow = fsCashFlow

instance HasAccumulators FinState where
  accumulators = fsAccumulators

{-
-- These cause an abundance of overlaps.  How to do this?
instance HasFinState s => HasTaxData s where
  taxData = finState . fsTaxData

instance HasFinState s => HasCashFlow s where
  cashFlow = finState . fsCashFlow

instance HasFinState s => HasAccumulators s where
  accumulators = finState . fsAccumulators
-}

class ReadsAccumulators s where
  getAccumulators :: Getter s Accumulators
  default getAccumulators :: HasAccumulators s => Getter s Accumulators
  getAccumulators = accumulators


instance Show FinState where
  show fs = "Net flow: " ++ show (fs ^. fsCashFlow) ++ "\n" ++ show (fs ^. fsTaxData) ++ "\n" ++ show (fs ^. fsAccumulators)

zeroFinState :: Currency -> FinState
zeroFinState c = FinState { _fsTaxData=defaultTaxData c,
                            _fsCashFlow=MV.zero c,
                            _fsAccumulators=newAccumulators }



addToAccumulator :: (MonadError FMCException m, ReadsExchangeRateFunction s, HasAccumulators s, MonadState s m) => AccumName -> MoneyValue -> m ()
addToAccumulator name amount = do
  when (T.null name) $ throwing _Other "No name specified in call to addToAccumulator"
  e <- use getExchangeRateFunction
  let f = MV.inFirst e (+)
--  let g (Accumulators a) = Accumulators $ M.insertWith f name a
  accumulators.accums %= M.insertWith f name amount

addToAccumulator' :: (MonadError FMCException m, ReadsExchangeRateFunction s, HasAccumulators s, MonadState s m) => AccumName -> MoneyValue -> m ()
addToAccumulator' name amount = do
  when (T.null name) $ throwing _Other "No name specified in call to addToAccumulator"
  e <- use getExchangeRateFunction
  let f = MV.inFirst e (+)
  accumulators.accums %= M.insertWith f name amount


getAccumulatedValue :: (MonadError FMCException m, ReadsAccumulators s, MonadState s m) => AccumName -> m MoneyValue
getAccumulatedValue accumName = do
  (Accumulators as) <- use getAccumulators
  noteM (FailedLookup ("Couldn't find accum named " <> (T.pack $ show accumName))) $ M.lookup accumName as

zeroAccumulator :: (MonadState s m, HasAccumulators s) => AccumName -> m ()
zeroAccumulator accumName = accumulators.accums %= M.delete accumName

{-
addCashFlow::(MonadState FinState m,MonadReader FinEnv m)=>MoneyValue->m ()
addCashFlow cf = do
  e <- view feExchange
  fsCashFlow %= MV.inFirst e (+) cf
-}

addCashFlow :: (HasCashFlow s, ReadsExchangeRateFunction s, MonadState s m) => MoneyValue -> m ()
addCashFlow cf = do
  e <- use getExchangeRateFunction
  cashFlow %= MV.inFirst e (+) cf
