{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Main where

import           Control.Lens                            (view, (^.))
import           Control.Monad                           (when)
import           Options.Applicative                     (execParser)

import           Data.List                               (elemIndex, transpose)
import           Data.Random.Source.PureMT               (PureMT, newPureMT,
                                                          pureMT)

import qualified Data.Map                                as M
import           Data.Maybe                              (fromJust)
import qualified Data.Vector                             as V
import           Data.Word                               (Word64)
import           System.IO                               (IOMode (WriteMode),
                                                          hClose, hPutStrLn,
                                                          openFile)
import           Text.Printf                             (printf)
import           Text.RawString.QQ                       (r)

import qualified FinancialMC.Parsers.Configuration       as C
import           FinancialMC.Parsers.ConfigurationLoader (buildInitialStateFromConfig,
                                                          loadConfigurations)
import           OptionParser                            (FinMCOptions (..),
                                                          finMCOptionParser)

import           FinancialMC.Base                        (BaseAsset, BaseFlow,
                                                          BaseLifeEvent,
                                                          BaseRateModelT,
                                                          BaseRule,
                                                          CombinedState,
                                                          DatedSummary (..),
                                                          FMCPathState,
                                                          FSSummary (..),
                                                          FinEnv,
                                                          HasCombinedState (..),
                                                          HasDatedSummary (..),
                                                          HasFSSummary (..),
                                                          HasMCState (..),
                                                          HasPathSummaryAndSeed (..),
                                                          LiquidityType (..),
                                                          PathState,
                                                          pattern PathState,
                                                          PathSummary,
                                                          PathSummaryAndSeed (..),
                                                          RandomSeed, doPaths,
                                                          doPathsIO, grossFlows,
                                                          isZeroNW, netWorth)

import           FinancialMC.Core.Analysis               (DatedMoneyValue (..), DatedSummaryWithReturns (..),
                                                          HasDatedMoneyValue (..),
                                                          HasSimHistories (..),
                                                          SimHistories (..),
                                                          addReturns,
                                                          analyzeBankruptcies,
                                                          historiesFromSummaries,
                                                          nwHistFromSummaries)
import           FinancialMC.Core.LifeEvent              (LifeEventConverters (LEC))
import           FinancialMC.Core.MoneyValue             (MoneyValue (MoneyValue))
import           FinancialMC.Core.Utilities              (Year, eitherToIO)

ccs::C.FMCComponentConverters BaseAsset BaseAsset BaseFlow BaseFlow BaseLifeEvent BaseLifeEvent BaseRule BaseRule BaseRateModelT BaseRateModelT
ccs = C.FMCComponentConverters id id id id id

lec::LifeEventConverters BaseAsset BaseFlow BaseLifeEvent
lec = LEC id id

type BasePathState = FMCPathState BaseAsset BaseFlow BaseLifeEvent BaseRule BaseRateModelT

runWithOptions :: BasePathState -> FinMCOptions -> IO [PathSummaryAndSeed]
runWithOptions ps0 options = do
  let showFS = optShowFinalStates options -- NB: if showFinalStates is true, paths will be run serially rather than in parallel
      logLevels = optLogLevels options
      srcF :: Maybe Word64 -> IO PureMT
      srcF (Just x) = return (pureMT x) -- if seed is present in options, use it.
      srcF Nothing  = newPureMT  -- otherwise, get pseudo-random seed via IO
      needsIO = not (null logLevels) || showFS
  src <- srcF (optSeed options)
  let runW x = x ps0 (optSingleThreaded options) src (optYears options) (optPaths options)
      pureThreaded = if (optSingleThreaded options) then "single" else "multi"
  if needsIO
    then putStrLn "Running IO (single-threaded) stack...\n" >> runW (doPathsIO lec logLevels showFS)
    else putStrLn ("Running pure (" ++ pureThreaded ++ "-threaded) stack...\n") >> (eitherToIO $ runW (doPaths lec))

parseOptions::IO FinMCOptions
parseOptions = execParser finMCOptionParser

outputPath :: Maybe String->Maybe String->Maybe String
outputPath Nothing Nothing = Nothing
outputPath (Just optPath) Nothing = Just optPath
outputPath Nothing (Just confPrefix) = Just confPrefix
outputPath (Just optPath) (Just confPrefix) = Just (optPath ++ "/" ++ confPrefix)

runAndOutput :: Bool -> FinMCOptions -> IO ()
runAndOutput doOutput options = do
  let writeIf x = when doOutput $ putStrLn x
  writeIf ("Running configs from " ++ optConfigFile options)
  (configInfo, configMap) <- loadConfigurations ccs (optSchemaPath options) (C.UnparsedFile (optConfigFile options))
  writeIf ("Loaded " ++ show (M.keys configMap))
  let runConfig :: String -> IO ()
      runConfig cfgName = do
        (mOPrefix, fe0, cs0) <- eitherToIO $ buildInitialStateFromConfig configInfo configMap cfgName
        let nw =  netWorth cs0 fe0
            (inFlow,outFlow) = grossFlows (cs0 ^. (csMC.mcsCashFlows)) fe0
        writeIf $ "Running config=" ++ cfgName
        when (optShowFinalStates options) . writeIf $ "Initial State: " ++ show cs0
        writeIf $ "Initial Net Worth: " ++ show nw
        writeIf $ "Initial positive cashflow: " ++ show inFlow
        writeIf $ "Initial gross spending: " ++ show outFlow

        summaries <- runWithOptions (PathState cs0 fe0) options
        let histData = nwHistFromSummaries summaries (optBins options)
            bankruptcies = view psasSummary <$> filter (isZeroNW . view psasSummary) summaries
            (numB,medianB,modeB) = analyzeBankruptcies bankruptcies
            pctB = (100*fromIntegral numB::Double)/fromIntegral (length summaries)
            strB1 = (show numB ++ " (" ++ printf "%0.2f" pctB ++ "%) paths result in bankruptcy.")
        writeIf strB1
        when (numB > 0) $ writeIf ("Median bankruptcy year=" ++ show (fromJust medianB) ++ "; mode=" ++ show (fromJust modeB))
        SimHistories nwHistories _ medianHist <- eitherToIO $ historiesFromSummaries lec summaries (fe0,cs0)
                                                            (optSingleThreaded options) (optQuantiles options) (optYears options)
        let DatedSummary _ medianFS = V.last medianHist
            mOPath = outputPath (optOutPath options) mOPrefix
        writeIf ("Median Final Summary=" ++ show medianFS)
        case mOPath of
             Nothing   -> writeIf "No saved output."
             Just path -> writeIf ("Saving output in " ++ path ++ "...")
        output mOPath histData nwHistories (medianFS ^. fssNW) medianHist

  mapM_ runConfig (optConfigs options)

normalMain::IO ()
normalMain = do
  parseOptions >>= runAndOutput True
  return ()

-- Criterion
{--
benchmarkOptions years paths = FinMCOptions years paths (Just 1) 30 5 Nothing [] False
benchmarkMain = defaultMain [
  bgroup "runAndOutput" [ bench "40yr 10path" $ nfIO (runAndOutput $ benchmarkOptions 40 10),
                          bench "40yr 50path" $ nfIO (runAndOutput $ benchmarkOptions 40 50),
                          bench "40yr 250path" $ nfIO (runAndOutput $ benchmarkOptions 40 250),
                          bench "10yr 250path" $ nfIO (runAndOutput $ benchmarkOptions 10 250),
                          bench "20yr 50path" $ nfIO (runAndOutput $ benchmarkOptions 20 250),
                          bench "40yr 250path" $ nfIO (runAndOutput $ benchmarkOptions 40 250)
                        ]
  ]
--}
main :: IO ()
main = normalMain


output :: Maybe String -> (V.Vector Double, V.Vector Int) -> [V.Vector DatedMoneyValue] -> MoneyValue -> V.Vector DatedSummary -> IO ()
output Nothing histData _ medianNW _ = do
  let histTSV = formatHistogramOutput histData
  putStrLn ("\n" ++ histTSV)
  putStrLn ("Median Final Net Worth " ++ show medianNW)


output (Just prefix) histData history _ medianHist = do
  let histogramTSV = formatHistogramOutput histData
  histogramHandle <- openFile (prefix ++ "_histogram.tsv") WriteMode
  hPutStrLn histogramHandle histogramTSV
  hClose histogramHandle
  historyHandle <- openFile (prefix ++ "_history.tsv") WriteMode
  hPutStrLn historyHandle $ formatHistoryOutput history
  hClose historyHandle
  let medianHistTSV = formatMedianSummaryOutput medianHist
  medianHistHandle <- openFile (prefix ++ "_median_details.tsv") WriteMode
  hPutStrLn medianHistHandle medianHistTSV
  hClose medianHistHandle
  let paths = V.sum (snd histData)
      quantiles = length history
  gnuplotHandle <- openFile (prefix ++ ".gnuplot") WriteMode
  hPutStrLn gnuplotHandle $ formatGnuplotOutput prefix paths quantiles
  hClose gnuplotHandle

divFloat :: (Integral a, Integral b) => a -> b -> Float
divFloat a b = (fromIntegral a::Float)/(fromIntegral b::Float)

formatHistogramOutput :: (V.Vector Double, V.Vector Int) -> String
formatHistogramOutput (lowerBound,count) = str where
  (sm,sumL) = foldl (\(s,l) n -> (s+(count V.! n),l++[s+(count V.! n)])) (0,[]) [0..(V.length count-1)]
  fmtLB x = printf "%.0f" (x/1000.0)
  str = foldl (\s n->s ++ fmtLB (lowerBound V.! n) ++ "\t" ++ show (count V.! n) ++ "\t" ++ show (divFloat (sumL !! n) sm) ++ "\n") "#k$\tcount\tCum Density\n" [0..(V.length lowerBound - 1)]

formatHistoryOutput :: [V.Vector DatedMoneyValue] -> String
formatHistoryOutput hists = str where
  histsL = V.toList <$> hists
  days = view dmYear <$> head histsL
  hists' = zip days (transpose (fmap (view dmValue) <$> histsL))
  header = foldl (\s n-> s ++ "Net Worth " ++ show n ++ "\t") "#Date\t" [1..length histsL] ++ "\n"
  fmtNWs = foldl (\s (MoneyValue x _) -> s ++ printf "%.0f" x ++ "\t") ""
  str = foldl (\s (d,nw)-> s ++ show d ++ "\t" ++ fmtNWs nw ++ "\n") header hists'

formatMedianSummaryOutput :: V.Vector DatedSummary -> String
formatMedianSummaryOutput medianHist = str where
  wrs = addReturns medianHist
  header = "Date\t\tNet Worth($)\tReturn($)\tReturn(%)\tNear Cash\tInflow($)\tOutflow($)\tTax($)\tTax Rate(%)\n"
  g = printf "%.2f"
  f (MoneyValue x _) = printf "%.0f" x
  l lt nwm = f (fromJust $ M.lookup lt nwm)
  h (DatedSummaryWithReturns d (FSSummary nw nwbo i o t tr)  ret retR)  =
    show d ++ "\t" ++ f nw ++ "\t" ++ f ret ++ "\t\t" ++ g (100*retR) ++ "%\t\t"
    ++ l NearCash nwbo ++ "\t\t"
    ++ f i ++ "\t\t" ++ f o ++ "\t\t" ++ f t ++ "\t" ++ g (100*tr) ++ "\n"
  str = V.foldl' (\s x -> s ++ h x) header wrs

formatGnuplotOutput :: String -> Int -> Int -> String
formatGnuplotOutput prefix paths quantiles = str where
  x = elemIndex '/' (reverse prefix)
  prefixWOPath = case x of
    Nothing -> prefix
    Just n  -> (reverse . fst) $ splitAt n (reverse prefix)
  f1 = "histogramFile=\"" ++ prefixWOPath ++ "_histogram.tsv\"\n"
  f2 = "historyFile=\"" ++ prefixWOPath ++ "_history.tsv\"\n"
  mpLayout = [r|set multiplot layout 2,1|] ++ "\n"
  histogramSetup = [r|set key top right
set ylabel "% in Bin"
set xlabel "Final Net Worth (in $1000s)"
set title "Final Net Worth: Histogram"|] ++ "\n"
  histogramPlot = "plot histogramFile u 1:($2*" ++ printf "%.2f" (divFloat (100::Int) paths) ++ ") with boxes t \"\"\n"
  historySetup = [r|set key top left
set ylabel "USD (in 1000s)"
set xlabel "Year"
set title "Sample history from each quantile"|] ++ "\n"
  qPct n q = printf "%.0f" (divFloat (100*(2*n - 1)) (2*q))
  historyPlot = "plot " ++ foldl (\s n -> s ++ " historyFile u 1:($" ++ show (n+1) ++ "/1000) with lines t \"" ++ qPct n quantiles ++ "%\",") "" [1..quantiles] ++ "\n"
  str =  f1 ++ f2 ++ mpLayout ++ histogramSetup ++ histogramPlot ++ historySetup ++ historyPlot

