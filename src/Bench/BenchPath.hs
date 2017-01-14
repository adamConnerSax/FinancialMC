module BenchPath
       (
         benchPathsIO
       ) where

import Criterion
import BenchUtils (bgroupM)

import Control.Exception (SomeException)
import Control.Lens ((^.))
import Data.Maybe (fromJust)
import System.Random.Mersenne.Pure64 (pureMT)
import Data.Word (Word64)

import FinancialMC.Base (CombinedState,HasCombinedState(..),FinEnv,HasMCState(..),PathSummary,execOnePathPure,doPaths)

import qualified FinancialMC.Parsers.Configuration as C
import FinancialMC.Parsers.ConfigurationLoader (loadConfigurations,buildInitialStateFromConfig)
import FinancialMC.Parsers.JSON.BaseTypes (baseParsers)

import FinancialMC.Core.Utilities (eitherToIO)


getSummaryS::Either SomeException (CombinedState,FinEnv)->Maybe PathSummary
getSummaryS x = case x of
  Left _ -> Nothing
  Right (cs,_) -> Just $ cs ^. csMC.mcsPathSummary

setupEnv::FilePath->String->IO (CombinedState,FinEnv)
setupEnv cFile cfgName = do
  (configInfo, configMap) <- loadConfigurations Nothing baseParsers (C.UnparsedFile cFile)
  (_,fe0,cs0) <- eitherToIO $ buildInitialStateFromConfig configInfo configMap cfgName
  return (cs0,fe0)

benchPathF::CombinedState->FinEnv->Int->PathSummary
benchPathF cs0 fe0  years = do
  fromJust . getSummaryS  $ execOnePathPure cs0 fe0 1 years


benchSinglePath::[(FilePath,[(String,String,[Int])])]->IO [Benchmark]
benchSinglePath  pathsToBench = do
  let fYears (cs,fe) name years = bench (name ++ "_" ++ show years) $ nf (benchPathF cs fe) years
      fcfg cFile cfgName benchName lYears = do
        s0 <- setupEnv cFile cfgName
        return $ bgroup benchName $ fmap (\y -> fYears s0 benchName y) lYears 
      fFile cFile cfgsToBench = mapM (\(cN,bN,lYears) -> fcfg cFile cN bN lYears) cfgsToBench
  mconcat <$> mapM (uncurry fFile) pathsToBench


getSummaryM::Either SomeException [(PathSummary,Word64)]->Maybe [PathSummary]
getSummaryM x = case x of
  Left _ -> Nothing
  Right result -> Just . fst . unzip $ result

benchMultiPathF::Bool->CombinedState->FinEnv->Int->Int->[PathSummary]
benchMultiPathF singleThreaded cs0 fe0 years paths =
  fromJust . getSummaryM $ doPaths cs0 fe0 singleThreaded (pureMT 1) years paths


benchMultiPath::Bool->[(FilePath,[(String,String,[Int],[Int])])]->IO [Benchmark]
benchMultiPath singleThreaded pathsToBench = do
  let fYP (cs,fe) name years paths = bench (name ++ "_" ++ show years ++ "_" ++ show paths) $ nf (benchMultiPathF singleThreaded cs fe years) paths
      fcfg cFile cfgName benchName lYears lPaths = do
        let yps = [(x,y) | x<-lYears, y<-lPaths]
        s0 <- setupEnv cFile cfgName
        return $ bgroup benchName $ fmap (\(y,p) -> fYP s0 benchName y p) yps
      fFile::FilePath->[(String,String,[Int],[Int])]->IO [Benchmark]
      fFile cFile cfgsToBench = mapM (\(cN,bN,lY,lP) -> fcfg cFile cN bN lY lP) cfgsToBench
  mconcat <$> mapM (uncurry fFile) pathsToBench


benchPathsIO::IO Benchmark
benchPathsIO = bgroup "Paths" <$> sequence 
               [ bgroup "Singles" <$> benchSinglePath
                 [
                   ("Configs/Tests/AssetTestConfigurations.yaml",[("BankTest","BankTest",[1,10])])
                 , ("/Users/adam/Documents/Planning/FMC/APConfig.yaml",[("Conservative","APCons",[1,50])])
                 ]
               , bgroup "Multis/SingleThreaded" <$> benchMultiPath True
                 [
                   ("Configs/Tests/AssetTestConfigurations.yaml",[("BankTest","BankTest",[1,50],[1,100])])
                 , ("/Users/adam/Documents/Planning/FMC/APConfig.yaml",[("Conservative","APCons",[1,50],[1,100])])
                 ]
               , bgroup "Multis/MultiThreaded" <$> benchMultiPath False
                 [
                   ("Configs/Tests/AssetTestConfigurations.yaml",[("BankTest","BankTest",[1,50],[1,100])])
                 , ("/Users/adam/Documents/Planning/FMC/APConfig.yaml",[("Conservative","APCons",[1,50],[1,100])])
                 ]
               ]
