{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
module BenchTypes where

import           FinancialMC.Base                        (BaseAsset, BaseFlow,
                                                          BaseLifeEvent,
                                                          BaseRateModelT,
                                                          BaseRule,
                                                          CombinedState,
                                                          FMCPathState, FinEnv,
                                                          HasCombinedState (..),
                                                          HasMCState (..),
                                                          PathState (..),
                                                          pattern PathState,
                                                          PathSummary)
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

type BenchCS = CombinedState BenchAsset BenchFlow BenchLifeEvent BenchRule
type BenchFE = FinEnv BenchRateModel


ccs :: C.FMCComponentConverters BaseAsset BenchAsset BaseFlow BenchFlow BaseLifeEvent BenchLifeEvent BaseRule BenchRule BaseRateModelT BenchRateModel
ccs = C.FMCComponentConverters id id id id id

type BenchPathState = FMCPathState BenchAsset BenchFlow BenchLifeEvent BenchRule BenchRateModel

lec :: LifeEventConverters BaseAsset BaseFlow BaseLifeEvent
lec = LEC id id

setupEnv :: FilePath -> String -> IO BenchPathState
setupEnv cFile cfgName = do
  (configInfo, configMap) <- loadConfigurations ccs Nothing (C.UnparsedFile cFile)
  (_,fe0,cs0) <- eitherToIO $ buildInitialStateFromConfig configInfo configMap cfgName
  return $ PathState cs0 fe0

getSummaryS :: Either FMCException BenchPathState -> Maybe PathSummary
getSummaryS x = case x of
  Left _                  -> Nothing
  Right (PathState cs  _) -> Just $ cs ^. csMC.mcsPathSummary




