{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies #-}
module BenchTypes where

import           FinancialMC.Base                        (BaseAsset, BaseFlow,
                                                          BaseLifeEvent,
                                                          BaseRateModelT,
                                                          BaseRule,
                                                          CombinedState,
                                                          FMCPathState(MkFMCPathState), FinEnv,
                                                          HasCombinedState (..),
                                                          HasMCState (..),
                                                          PathState (..),
                                                          pattern PathState,
                                                          ComponentTypes (..),
                                                          PathSummary)
import           FinancialMC.BaseComponents (BaseComponents)                 
import           FinancialMC.Core.LifeEvent              (LifeEventConverters (LEC))
import qualified FinancialMC.Parsers.Configuration       as C
import           FinancialMC.Parsers.ConfigurationLoader (buildInitialStateFromConfig,
                                                          loadConfigurations)

import           FinancialMC.Core.Utilities              (FMCException,
                                                          eitherToIO)

import           Control.Lens                            ((^.))

type BenchAsset = BaseAsset
type BenchFlow = BaseFlow
type BenchLifeEvent = BaseLifeEvent
type BenchRule = BaseRule
type BenchRateModel = BaseRateModelT

data BenchComponents
instance ComponentTypes BenchComponents where
  type AssetType BenchComponents = BenchAsset
  type FlowType BenchComponents = BenchFlow
  type LifeEventType BenchComponents = BenchLifeEvent
  type RuleType BenchComponents = BenchRule
  type RateModelType BenchComponents = BenchRateModel

type BenchCS = CombinedState BenchComponents --BenchAsset BenchFlow BenchLifeEvent BenchRule
type BenchFE = FinEnv BenchRateModel
  

ccs :: C.FMCComponentConverters BaseComponents BenchComponents
ccs = C.FMCComponentConverters id id id id id

type BenchPathState = FMCPathState BenchComponents

lec :: LifeEventConverters BaseAsset BaseFlow BenchLifeEvent
lec = LEC id id

setupEnv :: FilePath -> String -> IO BenchPathState
setupEnv cFile cfgName = do
  (configInfo, configMap) <- loadConfigurations ccs Nothing (C.UnparsedFile cFile)
  (_,fe0,cs0) <- eitherToIO $ buildInitialStateFromConfig configInfo configMap cfgName
  return $ MkFMCPathState (PathState cs0 fe0)

getSummaryS :: Either FMCException BenchPathState -> Maybe PathSummary
getSummaryS x = case x of
  Left _                  -> Nothing
  Right (MkFMCPathState (PathState cs  _)) -> Just $ cs ^. csMC.mcsPathSummary




