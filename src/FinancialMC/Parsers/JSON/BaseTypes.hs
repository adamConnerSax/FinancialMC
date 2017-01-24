{-# LANGUAGE OverloadedStrings,TemplateHaskell, DeriveGeneric, DeriveAnyClass, Rank2Types, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module FinancialMC.Parsers.JSON.BaseTypes
       (
       ) where

import           Control.Lens hiding ((.=))
import           Control.Monad.State (State,execState)

import qualified FinancialMC.Builders.Assets as Assets
import qualified FinancialMC.Builders.Flows as Flows
import qualified FinancialMC.Builders.Rules as Rules
import qualified FinancialMC.Builders.LifeEvents as LifeEvents
import qualified FinancialMC.Builders.RateModels as RateModels






