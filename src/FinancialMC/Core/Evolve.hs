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
import           FinancialMC.Core.Utilities       (AsFMCException (..),
                                                   FMCException (..))

--import           Control.Monad.Catch              (MonadThrow, SomeException,
--                                                   throwM)
import           Control.Monad.Except             (MonadError)
import           Control.Monad.Reader             (ReaderT, lift)

import           Control.Lens                     (Lens', magnify, mapMOf)
import           Control.Monad.Error.Lens         (throwing)
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

type EvolveApp rm = ResultT EvolveOutput (ReaderT (FinEnv rm) (Either FMCException))

type Evolver rm a = a -> EvolveApp rm a

liftER :: MV.ER (Either FMCException) a -> EvolveApp rm a
liftER = lift . magnify feExchange

class Evolvable e where
  evolve :: IsRateModel rm => Evolver rm e

evolveWithin :: IsRateModel rm => (Evolvable a, TR.Traversable t)=> b -> Lens' b (t a) -> EvolveApp rm b
evolveWithin oldB l = mapMOf (l.traverse) evolve oldB

applyTax ::( MonadError FMCException m
           , LoggableStepApp FinState ExchangeRateFunction m)
  => TaxAmount -> m ()
applyTax (TaxAmount tt y) =
  let taxFlow :: TaxDataApp (Either FMCException) ()
      taxFlow = addTaxableFlow tt y
      applyAction = taxDataApp2StepAppFSER taxFlow
      errorAction = throwing _Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then applyAction else errorAction

applyDeduction :: ( MonadError FMCException m
                  , LoggableStepApp FinState ExchangeRateFunction m)
  => TaxAmount -> m ()
applyDeduction (TaxAmount tt y) =
  let taxFlow :: TaxDataApp (Either FMCException) ()
      taxFlow = addDeductibleFlow tt y
      applyAction = taxDataApp2StepAppFSER taxFlow
      errorAction = throwing _Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then applyAction else errorAction

applyFlowAndTax :: ( MonadError FMCException m
                   , LoggableStepApp FinState ExchangeRateFunction m)
  => MoneyValue -> TaxAmount -> m ()
applyFlowAndTax x ta =   addCashFlow x >> applyTax ta

applyFlowAndDeduction :: ( MonadError FMCException m
                         , LoggableStepApp FinState ExchangeRateFunction m) => MoneyValue -> TaxAmount -> m ()
applyFlowAndDeduction x ta = addCashFlow x >> applyDeduction ta



applyFlow :: ( MonadError FMCException m
             , LoggableStepApp FinState ExchangeRateFunction m)
  => FlowResult
  -> m ()
applyFlow (UnTaxed x) = addCashFlow x
applyFlow (AllTaxed ta@(TaxAmount _ x)) = applyFlowAndTax x ta
applyFlow (AllDeductible ta@(TaxAmount _ x)) = applyFlowAndDeduction (MV.negate x) ta
applyFlow (OnlyTaxed ta) = applyTax ta
applyFlow (OnlyDeductible ta) = applyDeduction ta
applyFlow (PartiallyTaxed x ta) = applyFlowAndTax x ta
applyFlow (PartiallyDeductible x ta) = applyFlowAndDeduction x ta


applyFlows :: ( MonadError FMCException m
              , LoggableStepApp FinState ExchangeRateFunction m)
  => [FlowResult] -> m ()
applyFlows = mapM_ applyFlow


applyAccum :: ( MonadError FMCException m
              , LoggableStepApp FinState ExchangeRateFunction m) => AccumResult -> m ()
applyAccum (AddTo name amt) = addToAccumulator' name amt
applyAccum (Zero name)      = zeroAccumulator name


applyAccums :: ( MonadError FMCException m
               , LoggableStepApp FinState ExchangeRateFunction m) => [AccumResult] -> m ()
applyAccums  = mapM_ applyAccum


applyEvolveResult :: ( MonadError FMCException m
                     , LoggableStepApp FinState ExchangeRateFunction m) => EvolveResult a -> m a
applyEvolveResult (Result a (EvolveOutput flows accums)) = do
  applyFlows flows
  applyAccums accums
  return $! a

evolveAndApply :: forall rm m a. ( IsRateModel rm
                                 , MonadError FMCException m
                                 , Evolvable a
                                 , LoggableStepApp FinState (FinEnv rm) m)
  => a -> m a
evolveAndApply x = stepLift $  do
  let result :: ResultT EvolveOutput (ReaderT (FinEnv rm) (Either FMCException)) a
      result = evolve x
  x <- toStepApp $ lift $ runResultT result
  magnifyStepApp feExchange $ applyEvolveResult x
