{-# LANGUAGE MultiParamTypeClasses #-}
module BenchTypes where

import           FinancialMC.Base                        (BaseAsset, BaseFlow,
                                                          BaseLifeEvent,
                                                          BaseRateModelT,
                                                          BaseRule,
                                                          CombinedState, FinEnv,
                                                          HasCombinedState (..),
                                                          HasMCState (..),
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

--type BenchPathState = FMCPathState BenchAsset BenchFlow BenchLifeEvent BenchRule BenchRateModel

lec :: LifeEventConverters BaseAsset BaseFlow BaseLifeEvent
lec = LEC id id

setupEnv :: FilePath -> String -> IO (BenchCS, BenchFE)
setupEnv cFile cfgName = do
  (configInfo, configMap) <- loadConfigurations ccs Nothing (C.UnparsedFile cFile)
  (_,fe0,cs0) <- eitherToIO $ buildInitialStateFromConfig configInfo configMap cfgName
  return (cs0,fe0)

getSummaryS :: Either FMCException (BenchCS, BenchFE) -> Maybe PathSummary
getSummaryS x = case x of
  Left _       -> Nothing
  Right (cs,_) -> Just $ cs ^. csMC.mcsPathSummary

