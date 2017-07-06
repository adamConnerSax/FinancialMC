{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module FinancialMC.Core.Evolve
       (
         Evolver
       , EvolveApp
       , Evolvable(..)
       , EvolveOutput(..)
       , EvolveResult
       , TaxAmount(..)
       , FlowResult(..)
       , AccumResult(..)
       , liftER -- to match ReaderT ExchangeRateFunction Identity to Evolver
       , applyAccums --for Rules
       , applyFlows
       , evolveWithin
       , evolveAndApply
       ) where

import           FinancialMC.Core.FinApp          (LoggableStepApp (..),
                                                   StepLiftable (stepLift),
                                                   magnifyStepApp,
                                                   taxDataApp2StepAppFSER,
                                                   toStepApp)

import           Data.Monoid                      ((<>))
import           FinancialMC.Core.FinancialStates (AccumName, FinEnv, FinState,
                                                   HasFinEnv (..), addCashFlow,
                                                   addToAccumulator',
                                                   zeroAccumulator)
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction,
                                                   MoneyValue)
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Rates           (IsRateModel)
import           FinancialMC.Core.Result          (Result (Result), ResultT,
                                                   runResultT)
import           FinancialMC.Core.Tax             (TaxDataApp, TaxType,
                                                   addDeductibleFlow,
                                                   addTaxableFlow)
import           FinancialMC.Core.Utilities       (FMCException (..))

import           Control.Monad.Catch              (MonadThrow, SomeException,
                                                   throwM)
import           Control.Monad.Reader             (ReaderT, lift)

import           Control.Lens                     (Lens', magnify, mapMOf)
import qualified Data.Traversable                 as TR
--import qualified Data.Text as T


--The ability to modify the FinState as a result of Asset, Flow evolution (one year of return, divs, payments, salary, etc.) is all handled and specified here.
--An evolve function just returns a "Reader FinEnv EvolveResult" and the changes get applied to FinState from here (evolveAndApply) which gets called from evolveMCS
--This also constrains the returns, making it harder to forget to pair tax consequences with cashflows
data TaxAmount = TaxAmount !TaxType !MoneyValue deriving (Show)

deductibleAmount::TaxAmount->TaxAmount
deductibleAmount (TaxAmount tt x) = TaxAmount tt (MV.negate x)

data FlowResult = UnTaxed !MoneyValue |
                  AllTaxed !TaxAmount |
                  AllDeductible !TaxAmount |
                  OnlyTaxed !TaxAmount |
                  OnlyDeductible !TaxAmount |
                  PartiallyTaxed !MoneyValue !TaxAmount |
                  PartiallyDeductible !MoneyValue !TaxAmount
                deriving (Show)

data AccumResult = AddTo !AccumName !MoneyValue | Zero !AccumName deriving (Show)

data EvolveOutput = EvolveOutput ![FlowResult] ![AccumResult] deriving (Show)

instance Monoid EvolveOutput where
  mempty = EvolveOutput [] []
  mappend (EvolveOutput fAs aAs) (EvolveOutput fBs aBs) = EvolveOutput (fAs <> fBs) (aAs <> aBs)

type EvolveResult a = Result EvolveOutput a

type EvolveApp err rm = ResultT EvolveOutput (ReaderT (FinEnv rm) (Either err))

type Evolver err rm a = a -> EvolveApp err rm a

liftER :: MV.ER (Either err) a -> EvolveApp err rm a
liftER = lift . magnify feExchange

class Evolvable err e where
  evolve :: IsRateModel rm => Evolver err rm e


evolveWithin :: IsRateModel rm => (Evolvable err a, TR.Traversable t)=> b -> Lens' b (t a) -> EvolveApp err rm b
evolveWithin oldB l = mapMOf (l.traverse) evolve oldB


applyTax :: forall err m. (MonadThrow m, StepLiftable err FinState ExchangeRateFunction m) => TaxAmount -> m ()
applyTax (TaxAmount tt y) =
  let taxFlow :: TaxDataApp (Either err) ()
      taxFlow = addTaxableFlow tt y
      applyAction :: StepLiftable err FinState ExchangeRateFunction m => m ()
      applyAction = taxDataApp2StepAppFSER taxFlow
      errorAction:: MonadThrow m => m ()
      errorAction = throwM $ Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then applyAction else errorAction



applyDeduction :: forall err m. (MonadThrow m, StepLiftable err FinState ExchangeRateFunction m) => TaxAmount -> m ()
applyDeduction (TaxAmount tt y) =
  let taxFlow :: TaxDataApp (Either err) ()
      taxFlow = addDeductibleFlow tt y
      applyAction = taxDataApp2StepAppFSER taxFlow
      errorAction = throwM $ Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then applyAction else errorAction

applyFlowAndTax :: forall err m. ( MonadThrow m
                                 , LoggableStepApp err FinState ExchangeRateFunction m
                                 , StepLiftable err FinState ExchangeRateFunction m)
  => MoneyValue -> TaxAmount -> m ()
applyFlowAndTax x ta =
  let taxFlow :: (MonadThrow m, StepLiftable err FinState ExchangeRateFunction m) => m ()
      taxFlow = stepLift $ applyTax ta
  in addCashFlow x >> taxFlow


applyFlowAndDeduction :: (MonadThrow m, StepLiftable err FinState ExchangeRateFunction m) => MoneyValue -> TaxAmount -> m ()
applyFlowAndDeduction x ta = addCashFlow x >> applyDeduction ta



applyFlow :: (MonadThrow m, StepLiftable err FinState ExchangeRateFunction m)=>FlowResult -> m ()
applyFlow (UnTaxed x) = addCashFlow x
applyFlow (AllTaxed ta@(TaxAmount _ x)) = applyFlowAndTax x ta
applyFlow (AllDeductible ta@(TaxAmount _ x)) = applyFlowAndDeduction (MV.negate x) ta
applyFlow (OnlyTaxed ta) = applyTax ta
applyFlow (OnlyDeductible ta) = applyDeduction ta
applyFlow (PartiallyTaxed x ta) = applyFlowAndTax x ta
applyFlow (PartiallyDeductible x ta) = applyFlowAndDeduction x ta


applyFlows :: (MonadThrow m, StepLiftable err FinState ExchangeRateFunction m) => [FlowResult] -> app ()
applyFlows = mapM_ applyFlow


applyAccum :: (LoggableStepApp err FinState ExchangeRateFunction m) => AccumResult -> m ()
applyAccum (AddTo name amt) = addToAccumulator' name amt
applyAccum (Zero name)      = zeroAccumulator name


applyAccums :: (MonadThrow m, LoggableStepApp err FinState ExchangeRateFunction m) => [AccumResult] -> m ()
applyAccums  = mapM_ applyAccum


applyEvolveResult :: (MonadThrow m, LoggableStepApp err FinState ExchangeRateFunction app) => EvolveResult a -> m a
applyEvolveResult (Result a (EvolveOutput flows accums)) = do
  applyFlows flows
  applyAccums accums
  return $! a

evolveAndApply :: IsRateModel rm => (Evolvable err a, LoggableStepApp err FinState (FinEnv rm) app)=> a -> m a
evolveAndApply a = stepLift $  do
  x <- (toStepApp . lift . runResultT . evolve $ a)
  magnifyStepApp feExchange . applyEvolveResult $ x
