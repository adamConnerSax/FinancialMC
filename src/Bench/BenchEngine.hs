{-# LANGUAGE MultiParamTypeClasses #-}
module BenchEngine
  (
    benchEngineIO
  ) where

import           BenchTypes
import           FinancialMC.Base           (PathSummary)
import           FinancialMC.Core.Utilities (FMCException)
import           FinancialMC.Internal       (PathStack, computeTax, doChecks,
                                             doTax, evolveMCS, execPathStack)

import           Control.Monad              (replicateM_)
import           Criterion
import           Data.Maybe                 (fromJust)

runEngineFunction :: BenchPathState -> PathStack FMCException BenchCS BenchFE x -> PathSummary
runEngineFunction ps0 ef = do
  fromJust . getSummaryS  $ execPathStack ef ps0

benchDoChecks :: BenchPathState -> Int -> PathSummary
benchDoChecks ps0 n =
  let ef = replicateM_ n doChecks
  in runEngineFunction ps0 ef

benchComputeTax :: BenchPathState -> Int -> PathSummary
benchComputeTax ps0 n =
  let ef = replicateM_ n computeTax
  in runEngineFunction ps0 ef

benchDoTax :: BenchPathState -> Int -> PathSummary
benchDoTax ps0 n =
  let ef = replicateM_ n doTax
  in runEngineFunction ps0 ef

benchEvolveMCS :: BenchPathState -> Int -> PathSummary
benchEvolveMCS ps0 n =
  let ef = replicateM_ n evolveMCS
  in runEngineFunction ps0 ef

benchEngineF ::  (String, (BenchPathState -> Int -> PathSummary)) -> [(FilePath, [(String, String, [Int])])] -> IO [Benchmark]
benchEngineF (efName,ef) pathsToBench = do
  let fYP bps name iters = bench (efName ++ "_" ++ show iters) $ nf (ef bps) iters
      fcfg cFile cfgName benchName lIters = do
--        let yps = [(x,y) | x<-lYears, y<-lPaths]
        s0 <- setupEnv cFile cfgName
        return $ bgroup benchName $ fmap (\n -> fYP s0 benchName n) lIters
      fFile :: FilePath -> [(String, String, [Int])] -> IO [Benchmark]
      fFile cFile cfgsToBench = mapM (\(cN,bN,lI) -> fcfg cFile cN bN lI) cfgsToBench
  mconcat <$> mapM (uncurry fFile) pathsToBench

pathConfigs :: [(FilePath, [(String, String, [Int])])]
pathConfigs =
  [
    ("Configs/Tests/AssetTestConfigurations.xml",[("BankTest","BankTest",[1,10])])
  , ("/Users/adam/Documents/Planning/FMC/APConfig.xml",[("Conservative","APCons",[1,10])])
  ]

engineFs :: [(String, (BenchPathState -> Int -> PathSummary))]
engineFs = [ ("doCheck", benchDoChecks)
           , ("computeTax", benchComputeTax)
           , ("doTax",benchDoTax)
           , ("evolveMCS",benchEvolveMCS)
           ]

benchEngineIO :: IO Benchmark
benchEngineIO = bgroup "Engine" . mconcat <$> (sequence $ fmap (flip benchEngineF pathConfigs) engineFs)
