{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

import           FinancialMC.Core.FinApp (LoggableStepApp(..),toStepApp,taxDataApp2StepAppFSER,magnifyStepApp)

import           FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..),FinState,AccumName,addToAccumulator',zeroAccumulator,addCashFlow)
import           FinancialMC.Core.MoneyValue (MoneyValue,ExchangeRateFunction)
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Tax (TaxType,addTaxableFlow,addDeductibleFlow)
import           FinancialMC.Core.Result (Result(Result),ResultT,runResultT)
import           FinancialMC.Core.Utilities (FMCException(..))
import           FinancialMC.Core.Rates (IsRateModel)
import           Data.Monoid ((<>))

import           Control.Monad.Reader (ReaderT,lift)
import           Control.Monad.Catch (SomeException,MonadThrow,throwM)

import           Control.Lens (magnify,Lens',mapMOf)
import qualified Data.Traversable as TR
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
  mappend (EvolveOutput fAs aAs) (EvolveOutput fBs aBs) = EvolveOutput (fAs<>fBs) (aAs<>aBs)  

type EvolveResult a = Result EvolveOutput a 

type EvolveApp rm = ResultT EvolveOutput (ReaderT (FinEnv rm) (Either SomeException))

type Evolver rm a = a->EvolveApp rm a 
                 
liftER::MV.ER (Either SomeException) a->EvolveApp rm a
liftER = lift . magnify feExchange

class Evolvable e where
  evolve::IsRateModel rm=>Evolver rm e


evolveWithin::IsRateModel rm=>(Evolvable a,TR.Traversable t)=>b->Lens' b (t a)->EvolveApp rm b 
evolveWithin oldB l = mapMOf (l.traverse) evolve oldB


applyTax::(LoggableStepApp FinState ExchangeRateFunction app)=>TaxAmount->app ()  
applyTax (TaxAmount tt y) = stepLift $ if MV.isNonNegative y 
                then taxDataApp2StepAppFSER $ addTaxableFlow tt y 
                else throwM $ Other "Can't call applyTax with a -ve number."


applyDeduction::(LoggableStepApp FinState ExchangeRateFunction app)=>TaxAmount->app ()
applyDeduction (TaxAmount tt y) = stepLift $ if MV.isNonNegative y 
                      then taxDataApp2StepAppFSER $ addDeductibleFlow tt y                      
                      else throwM $ Other "Cant call applyDeduction with a -ve number"


applyFlowAndTax::(MonadThrow app, LoggableStepApp FinState ExchangeRateFunction app)=>MoneyValue->TaxAmount->app ()  
applyFlowAndTax x ta = addCashFlow x >> applyTax ta


applyFlowAndDeduction::(MonadThrow app, LoggableStepApp FinState ExchangeRateFunction app)=>MoneyValue->TaxAmount->app ()  
applyFlowAndDeduction x ta = addCashFlow x >> applyDeduction ta



applyFlow::(LoggableStepApp FinState ExchangeRateFunction app)=>FlowResult -> app ()
applyFlow (UnTaxed x) = addCashFlow x
applyFlow (AllTaxed ta@(TaxAmount _ x)) = applyFlowAndTax x ta   
applyFlow (AllDeductible ta@(TaxAmount _ x)) = applyFlowAndDeduction (MV.negate x) ta
applyFlow (OnlyTaxed ta) = applyTax ta 
applyFlow (OnlyDeductible ta) = applyDeduction ta 
applyFlow (PartiallyTaxed x ta) = applyFlowAndTax x ta
applyFlow (PartiallyDeductible x ta) = applyFlowAndDeduction x ta


applyFlows::(MonadThrow app, LoggableStepApp FinState ExchangeRateFunction app)=>[FlowResult]->app ()
applyFlows = mapM_ applyFlow
  

applyAccum::(LoggableStepApp FinState ExchangeRateFunction app)=>AccumResult->app ()
applyAccum (AddTo name amt) = addToAccumulator' name amt 
applyAccum (Zero name) = zeroAccumulator name


applyAccums::(MonadThrow app, LoggableStepApp FinState ExchangeRateFunction app)=>[AccumResult]->app ()
applyAccums  = mapM_ applyAccum


applyEvolveResult::(MonadThrow app, LoggableStepApp FinState ExchangeRateFunction app)=>EvolveResult a -> app a
applyEvolveResult (Result a (EvolveOutput flows accums)) = do
  applyFlows flows
  applyAccums accums
  return $! a

evolveAndApply::IsRateModel rm=>(Evolvable a,LoggableStepApp FinState (FinEnv rm) app)=> a->app a
evolveAndApply a = stepLift $  do
  x <- (toStepApp . lift . runResultT . evolve $ a)
  magnifyStepApp feExchange . applyEvolveResult $ x
