{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BenchEngine
  (
    benchEngineIO
  ) where

import           BenchTypes
import           FinancialMC.Base           (CombinedState, FinEnv, MoneyValue,
                                             PathSummary, csFinancial)

import           FinancialMC.Core.Utilities (FMCException)
import           FinancialMC.Internal       (EngineC, HasCombinedState (..),
                                             HasFinState (..),
                                             LoggablePathApp (stepToPath),
                                             LoggableStepApp, PathApp, StepApp,
                                             computeTax, doTax, evolveMCS,
                                             execPathApp, zoomStepApp)

import           Control.Monad              (replicateM_)
import           Criterion
import           Data.Maybe                 (fromJust)

type BenchEngineC m = ( EngineC BenchAsset BenchFlow BenchLifeEvent BenchRule BenchRateModel
                      , LoggableStepApp (CombinedState BenchAsset BenchFlow BenchLifeEvent BenchRule) (FinEnv BenchRateModel) m)

runStepFunction :: BenchCS -> BenchFE -> StepApp FMCException BenchCS BenchFE x -> PathSummary
runStepFunction cs0 fe0 ef = do
  fromJust . getSummaryS  $ execPathApp (stepToPath ef) cs0 fe0

benchComputeTax :: BenchCS -> BenchFE  -> Int -> PathSummary
benchComputeTax cs0 fe0 n =
  let ef = replicateM_ n $ zoomStepApp csFinancial $ zoomStepApp fsTaxData $ computeTax
  in runStepFunction cs0 fe0 ef

benchDoTax :: BenchCS -> BenchFE  -> Int -> PathSummary
benchDoTax cs0 fe0 n =
  let ef = replicateM_ n doTax
  in runStepFunction cs0 fe0 ef

benchEvolveMCS :: BenchCS -> BenchFE -> Int -> PathSummary
benchEvolveMCS cs0 fe0 n =
  let ef = replicateM_ n evolveMCS
  in runStepFunction cs0 fe0 ef


benchEngineF ::  (String, (BenchCS -> BenchFE -> Int -> PathSummary)) -> [(FilePath, [(String, String, [Int])])] -> IO [Benchmark]
benchEngineF (efName,ef) pathsToBench = do
  let fYP cs fe name iters = bench (efName ++ "_" ++ show iters) $ nf (ef cs fe) iters
      fcfg cFile cfgName benchName lIters = do
--        let yps = [(x,y) | x<-lYears, y<-lPaths]
        (cs0, fe0) <- setupEnv cFile cfgName
        return $ bgroup benchName $ fmap (\n -> fYP cs0 fe0 benchName n) lIters
      fFile :: FilePath -> [(String, String, [Int])] -> IO [Benchmark]
      fFile cFile cfgsToBench = mapM (\(cN,bN,lI) -> fcfg cFile cN bN lI) cfgsToBench
  mconcat <$> mapM (uncurry fFile) pathsToBench

pathConfigs :: [(FilePath, [(String, String, [Int])])]
pathConfigs =
  [
    ("Configs/Tests/AssetTestConfigurations.xml",[("BankTest","BankTest",[1,10])])
  , ("/Users/adam/Documents/Planning/FMC/APConfig.xml",[("Conservative","APCons",[1,10])])
  ]

engineFs :: [(String, (BenchCS -> BenchFE -> Int -> PathSummary))]
engineFs = [ ("computeTax", benchComputeTax)
           , ("doTax", benchDoTax)
           , ("evolveMCS", benchEvolveMCS)
           ]

benchEngineIO :: IO Benchmark
benchEngineIO = bgroup "Engine" . mconcat <$> (sequence $ fmap (flip benchEngineF pathConfigs) engineFs)
