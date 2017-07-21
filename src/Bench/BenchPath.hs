{-# LANGUAGE MultiParamTypeClasses #-}
module BenchPath
       (
         benchPathsIO
       ) where
import           BenchTypes

import           BenchUtils                        (bgroupM)
import           Criterion

--import Control.Exception (SomeException)
import           Control.Lens                      (view, (^.))
import           Data.Maybe                        (fromJust)
import           Data.Word                         (Word64)
import           FinancialMC.Core.LifeEvent        (LifeEventConverters (..))
import           System.Random.Mersenne.Pure64     (pureMT)

import           FinancialMC.Base                  (BaseAsset, BaseFlow,
                                                    BaseLifeEvent,
                                                    BaseRateModelT, BaseRule,
                                                    CombinedState, FMCPathState,
                                                    FinEnv,
                                                    HasCombinedState (..),
                                                    HasMCState (..),
                                                    HasPathSummaryAndSeed (..),
                                                    PathState, PathSummary,
                                                    PathSummaryAndSeed, doPaths,
                                                    execOnePathPure)

import qualified FinancialMC.Parsers.Configuration as C



import           FinancialMC.Core.Utilities        (FMCException, eitherToIO)


benchPathF :: BenchPathState -> Int -> PathSummary
benchPathF bps years = do
  fromJust . getSummaryS  $ execOnePathPure lec bps 1 years


benchSinglePath :: [(FilePath, [(String, String, [Int])])] -> IO [Benchmark]
benchSinglePath  pathsToBench = do
  let fYears bps name years = bench (name ++ "_" ++ show years) $ nf (benchPathF bps) years
      fcfg cFile cfgName benchName lYears = do
        s0 <- setupEnv cFile cfgName
        return $ bgroup benchName $ fmap (\y -> fYears s0 benchName y) lYears
      fFile cFile cfgsToBench = mapM (\(cN,bN,lYears) -> fcfg cFile cN bN lYears) cfgsToBench
  mconcat <$> mapM (uncurry fFile) pathsToBench


getSummaryM :: Either FMCException [PathSummaryAndSeed] -> Maybe [PathSummary]
getSummaryM x = case x of
  Left _       -> Nothing
  Right result -> Just $ view psasSummary <$> result

benchMultiPathF :: Bool -> BenchPathState -> Int -> Int -> [PathSummary]
benchMultiPathF singleThreaded bps0 years paths =
  fromJust . getSummaryM $ doPaths lec bps0 singleThreaded (pureMT 1) years paths


benchMultiPath :: Bool -> [(FilePath, [(String, String, [Int], [Int])])] -> IO [Benchmark]
benchMultiPath singleThreaded pathsToBench = do
  let fYP bps name years paths = bench (name ++ "_" ++ show years ++ "_" ++ show paths) $ nf (benchMultiPathF singleThreaded bps years) paths
      fcfg cFile cfgName benchName lYears lPaths = do
        let yps = [(x,y) | x<-lYears, y<-lPaths]
        s0 <- setupEnv cFile cfgName
        return $ bgroup benchName $ fmap (\(y,p) -> fYP s0 benchName y p) yps
      fFile :: FilePath -> [(String, String, [Int], [Int])] -> IO [Benchmark]
      fFile cFile cfgsToBench = mapM (\(cN,bN,lY,lP) -> fcfg cFile cN bN lY lP) cfgsToBench
  mconcat <$> mapM (uncurry fFile) pathsToBench


benchPathsIO :: IO Benchmark
benchPathsIO = bgroup "Paths" <$> sequence
               [ bgroup "Singles" <$> benchSinglePath
                 [
                   ("Configs/Tests/AssetTestConfigurations.xml",[("BankTest","BankTest",[1,10])])
                 , ("/Users/adam/Documents/Planning/FMC/APConfig.xml",[("Conservative","APCons",[1,50])])
                 ]
               , bgroup "Multis/SingleThreaded" <$> benchMultiPath True
                 [
                   ("Configs/Tests/AssetTestConfigurations.xml",[("BankTest","BankTest",[1,50],[1,100])])
                 , ("/Users/adam/Documents/Planning/FMC/APConfig.xml",[("Conservative","APCons",[1,50],[1,100])])
                 ]
               , bgroup "Multis/MultiThreaded" <$> benchMultiPath False
                 [
                   ("Configs/Tests/AssetTestConfigurations.xml",[("BankTest","BankTest",[1,50],[1,100])])
                 , ("/Users/adam/Documents/Planning/FMC/APConfig.xml",[("Conservative","APCons",[1,50],[1,100])])
                 ]
               ]
