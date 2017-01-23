{-# LANGUAGE OverloadedStrings,TemplateHaskell, DeriveGeneric, DeriveAnyClass, Rank2Types, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module FinancialMC.Parsers.JSON.BaseTypes
       (
         ParserMap
       , FMC_ParserMaps
       , addParser
       , baseParsers
       ) where

import           Control.Lens hiding ((.=))
import           Control.Monad.State (State,execState)
-- import           Data.Aeson (ToJSON(toJSON),FromJSON(parseJSON),object,(.=),(.:),Value(Object))
import           Data.Aeson.Existential (ParseF,HasParsers(parserLookup),makeSimpleParserPairG,makeEnvParserPairG)
-- import           Data.Time (Day,fromGregorian,toGregorian)

--import           FinancialMC.Core.Asset (Asset(MkAsset))
--import           FinancialMC.Core.Flow  (Flow(MkFlow))
--import           FinancialMC.Core.Rule (Rule(MkRule))
--import           FinancialMC.Core.LifeEvent (LifeEvent(MkLifeEvent))
import           FinancialMC.Core.Rates (RateModel(MkRateModel))
import qualified FinancialMC.Builders.Assets as Assets
import qualified FinancialMC.Builders.Flows as Flows
import qualified FinancialMC.Builders.Rules as Rules
import qualified FinancialMC.Builders.LifeEvents as LifeEvents
import qualified FinancialMC.Builders.RateModels as RateModels
import           FinancialMC.Builders.RateModels (RateModelFactor(MkRateModelFactor))

import qualified Data.Map as M

{-
instance ToJSON Day where
  toJSON day = object ["y" .= y, "m" .= m, "d" .= d] where
    (y,m,d) = toGregorian day
    
instance FromJSON Day where
  parseJSON (Object v) = fromGregorian 
                         <$> v .: "y"
                         <*> v .: "m"
                         <*> v .: "d"
  parseJSON _ = fail "non-object given to parseJSON :: Parser Day"
-}
type ParserMap e a = M.Map String (ParseF e a)

data FMC_ParserMaps =
  FMC_ParserMaps
  {
--      _assetParsers::ParserMap FMC_ParserMaps Asset
--    _flowParsers::ParserMap FMC_ParserMaps Flow
--    _ruleParsers::ParserMap FMC_ParserMaps Rule
--  , _lifeEventParsers::ParserMap FMC_ParserMaps LifeEvent
    _rateModelParsers::ParserMap FMC_ParserMaps RateModel
  , _rateModelFactorParsers::ParserMap FMC_ParserMaps RateModelFactor
  }

makeClassy ''FMC_ParserMaps

type ParserMapLens a = Lens' FMC_ParserMaps (ParserMap FMC_ParserMaps a)

{-
instance HasParsers FMC_ParserMaps Asset where
  parserLookup key x = M.lookup key (_assetParsers x)

instance HasParsers FMC_ParserMaps LifeEvent where
  parserLookup key x = M.lookup key (_lifeEventParsers x)

instance HasParsers FMC_ParserMaps Flow where
  parserLookup key x = M.lookup key (_flowParsers x)

instance HasParsers FMC_ParserMaps Rule where
  parserLookup key x = M.lookup key (_ruleParsers x)
-}

instance HasParsers FMC_ParserMaps RateModel where
  parserLookup key x = M.lookup key (_rateModelParsers x)

instance HasParsers FMC_ParserMaps RateModelFactor where
  parserLookup key x = M.lookup key (_rateModelFactorParsers x)


addParser::ParserMapLens a->(String,ParseF FMC_ParserMaps a)->State FMC_ParserMaps ()
addParser l (key,parser) = l %= M.insert key parser

addBaseParsers::State FMC_ParserMaps ()
addBaseParsers = do
{-
  addParser assetParsers $ makeSimpleParserPairG (MkAsset :: Assets.CashAsset->Asset)
  addParser assetParsers $ makeSimpleParserPairG (MkAsset :: Assets.MixedFund->Asset)
  addParser assetParsers $ makeSimpleParserPairG (MkAsset :: Assets.ResidentialRE->Asset)
  addParser assetParsers $ makeSimpleParserPairG (MkAsset :: Assets.FixedRateMortgage->Asset)
  addParser assetParsers $ makeSimpleParserPairG (MkAsset :: Assets.GuaranteedFund->Asset)
  addParser lifeEventParsers  $ makeSimpleParserPairG (MkLifeEvent :: LifeEvents.BuyProperty->LifeEvent)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.Expense->Flow)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.DeductibleExpense->Flow)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.EducationalExpense->Flow)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.HealthCareExpense->Flow)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.Payment->Flow)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.SalaryPayment->Flow)
  addParser flowParsers  $ makeSimpleParserPairG (MkFlow :: Flows.RentalIncome->Flow)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.PayFrom->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.Transfer->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.Contribution->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.RequiredDistribution->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.CashToInvestmentSweep->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.SellAsNeeded->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.Sweep->Rule)
  addParser ruleParsers  $ makeSimpleParserPairG (MkRule :: Rules.TaxTrade->Rule)
-}
  addParser rateModelParsers  $ makeEnvParserPairG (MkRateModel :: RateModels.SingleFactorModel->RateModel)
  addParser rateModelParsers  $ makeEnvParserPairG (MkRateModel :: RateModels.GroupedFactorModel->RateModel)
  addParser rateModelParsers  $ makeEnvParserPairG (MkRateModel :: RateModels.SameFactorModel->RateModel)
  addParser rateModelParsers  $ makeEnvParserPairG (MkRateModel :: RateModels.ListModel->RateModel)
  addParser rateModelFactorParsers  $ makeSimpleParserPairG (MkRateModelFactor :: RateModels.FixedRateModelFactor->RateModelFactor)
  addParser rateModelFactorParsers  $ makeSimpleParserPairG (MkRateModelFactor :: RateModels.NormalRateModelFactor->RateModelFactor)
  addParser rateModelFactorParsers  $ makeSimpleParserPairG (MkRateModelFactor :: RateModels.LogNormalRateModelFactor->RateModelFactor)

baseParsers::FMC_ParserMaps
baseParsers = execState addBaseParsers (FMC_ParserMaps M.empty M.empty)




