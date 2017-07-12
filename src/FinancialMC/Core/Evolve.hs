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
       , EvolveC
       , Evolvable(..)
       , EvolveOutput(..)
       , EvolveResult
       , TaxAmount(..)
       , FlowResult(..)
       , AccumResult(..)
       , applyAccums --for Rules
       , applyFlows
       , evolveWithin
       , evolveAndApply
       ) where

--import           FinancialMC.Core.FinApp          (PathStack)

import           Data.Monoid                      ((<>))
import           FinancialMC.Core.FinancialStates (AccumName, FinEnv, FinState, HasAccumulators (accumulators),
                                                   HasCashFlow (cashFlow),
                                                   HasFinState (..),
                                                   HasTaxData (..),
                                                   ReadsFinEnv (..),
                                                   addCashFlow,
                                                   addToAccumulator',
                                                   zeroAccumulator)
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction,
                                                   MoneyValue,
                                                   ReadsExchangeRateFunction)
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Rates           (IsRateModel, Rate,
                                                   ReadsRateTable)
import           FinancialMC.Core.Result          (MonadResult, Result (Result),
                                                   runResultT)
import           FinancialMC.Core.Tax             (TaxDataAppC, TaxType,
                                                   addDeductibleFlow,
                                                   addTaxableFlow)
import           FinancialMC.Core.Utilities       (AsFMCException (..),
                                                   FMCException (..))

import           Control.Monad.Except             (MonadError)
--import           Control.Monad.Reader             (ReaderT, lift)
import           Control.Monad.State              (MonadState)

import           Control.Lens                     (Lens', mapMOf)
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

type EvolveC s rm m = ( MonadError FMCException m
                      , MonadState s m
                      , ReadsExchangeRateFunction s
                      , ReadsRateTable s Rate -- this ought to come for free from ReadsFinEnv.  How to do?
                      , IsRateModel rm
                      , ReadsFinEnv s rm)

--type EvolveApp rm = ResultT EvolveOutput (ReaderT (FinEnv rm) (Either FMCException))

type Evolver s rm m a = (EvolveC s rm m, MonadResult EvolveOutput m) => a -> m a

--liftER :: (MonadState s m, ReadsExchangeRateFunction s) => MV.ER (Either FMCException) a -> m a
--liftER = readOnly

class Evolvable a where
  evolve :: (EvolveC s rm m, MonadResult EvolveOutput m) => a -> m a

evolveWithin :: (EvolveC s rm m, MonadResult EvolveOutput m, Evolvable a, TR.Traversable t) => b -> Lens' b (t a) -> m b
evolveWithin oldB l = mapMOf (l.traverse) evolve oldB


applyTax :: ( MonadError FMCException m
            , MonadState s m
            , HasTaxData s
            , ReadsExchangeRateFunction s) => TaxAmount -> m ()
applyTax (TaxAmount tt y) =
  let taxFlow = addTaxableFlow tt y
      onError = throwing _Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then taxFlow else onError

{-
applyTax ::( MonadError FMCException m
           , LoggableStepApp FinState ExchangeRateFunction m)
  => TaxAmount -> m ()
applyTax (TaxAmount tt y) =
  let taxFlow :: TaxDataApp (Either FMCException) ()
      taxFlow = addTaxableFlow tt y
      applyAction = taxDataApp2StepAppFSER taxFlow
      errorAction = throwing _Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then applyAction else errorAction
-}

applyDeduction :: ( MonadError FMCException m
                  , MonadState s m
                  , HasTaxData s
                  , ReadsExchangeRateFunction s) => TaxAmount -> m ()
applyDeduction (TaxAmount tt y) =
  let taxFlow = addDeductibleFlow tt y
      errorAction = throwing _Other "Can't call applyTax with a -ve number."
  in if MV.isNonNegative y then taxFlow else errorAction

--

applyFlowAndTax :: ( MonadError FMCException m
                   , MonadState s m
                   , HasTaxData s
                   , HasCashFlow s
                   , ReadsExchangeRateFunction s) => MoneyValue -> TaxAmount -> m ()
applyFlowAndTax x ta =   addCashFlow x >> applyTax ta

applyFlowAndDeduction :: ( MonadError FMCException m
                         , MonadState s m
                         , HasTaxData s
                         , HasCashFlow s
                         , ReadsExchangeRateFunction s) => MoneyValue -> TaxAmount -> m ()
applyFlowAndDeduction x ta = addCashFlow x >> applyDeduction ta

applyFlow :: ( MonadError FMCException m
             , MonadState s m
             , HasTaxData s
             , HasCashFlow s
             , ReadsExchangeRateFunction s) => FlowResult -> m ()
applyFlow (UnTaxed x) = addCashFlow x
applyFlow (AllTaxed ta@(TaxAmount _ x)) = applyFlowAndTax x ta
applyFlow (AllDeductible ta@(TaxAmount _ x)) = applyFlowAndDeduction (MV.negate x) ta
applyFlow (OnlyTaxed ta) = applyTax ta
applyFlow (OnlyDeductible ta) = applyDeduction ta
applyFlow (PartiallyTaxed x ta) = applyFlowAndTax x ta
applyFlow (PartiallyDeductible x ta) = applyFlowAndDeduction x ta

applyFlows :: ( MonadError FMCException m
              , MonadState s m
              , HasTaxData s
              , HasCashFlow s
              , ReadsExchangeRateFunction s) => [FlowResult] -> m ()
applyFlows = mapM_ applyFlow

applyAccum :: ( MonadError FMCException m
              , MonadState s m
              , HasAccumulators s
              , ReadsExchangeRateFunction s) => AccumResult -> m ()
applyAccum (AddTo name amt) = addToAccumulator' name amt
applyAccum (Zero name)      = zeroAccumulator name


applyAccums :: ( MonadError FMCException m
               , MonadState s m
               , HasAccumulators s
               , ReadsExchangeRateFunction s) => [AccumResult] -> m ()
applyAccums  = mapM_ applyAccum


-- ought to be able to replace HasTaxData, HasCashFlow and HasAccumulators with HasFinState.  THey are isomorphic.  But how?
applyEvolveResult :: ( MonadError FMCException m
                     , MonadState s m
                     , HasTaxData s
                     , HasCashFlow s
                     , HasAccumulators s
                     , ReadsExchangeRateFunction s) => EvolveResult a -> m a
applyEvolveResult (Result a (EvolveOutput flows accums)) = do
  applyFlows flows
  applyAccums accums
  return $! a

-- ought to be able to replace HasTaxData, HasCashFlow and HasAccumulators with HasFinState.  THey are isomorphic.  But how?
evolveAndApply :: ( Evolvable a
                  , EvolveC s rm m
                  , HasTaxData s
                  , HasCashFlow s
                  , HasAccumulators s) => a -> m a
evolveAndApply x = do
  x <- runResultT $ evolve x
  applyEvolveResult x
