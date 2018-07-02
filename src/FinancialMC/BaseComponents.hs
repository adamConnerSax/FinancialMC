{-# LANGUAGE TypeFamilies #-}
module FinancialMC.BaseComponents
  (
    ComponentTypes(..)
  , BaseComponents
  ) where

import           FinancialMC.Builders.Assets     (BaseAsset)
import           FinancialMC.Builders.Flows      (BaseFlow)
import           FinancialMC.Builders.LifeEvents (BaseLifeEvent)
import           FinancialMC.Builders.RateModels (BaseRateModelT)
import           FinancialMC.Builders.Rules      (BaseRule)

import           FinancialMC.Core.MCState        (ComponentTypes (..))

data BaseComponents

instance ComponentTypes BaseComponents where
  type AssetType BaseComponents = BaseAsset
  type FlowType BaseComponents = BaseFlow
  type LifeEventType BaseComponents = BaseLifeEvent
  type RuleType BaseComponents = BaseRule
  type RateModelType BaseComponents = BaseRateModelT



