{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module FinancialMC.Core.Analysis 
         (
           summariesToHistogram
         , nwHistFromSummaries
         , historiesFromSummaries
         , addReturns
         , analyzeBankruptcies
         ) where

import FinancialMC.Core.MCState (FSSummary(..),HasFSSummary(..),netWorthBreakout)
import FinancialMC.Core.MoneyValue (MoneyValue(..),HasMoneyValue(..))
import FinancialMC.Core.LifeEvent (LifeEventConverters)
import FinancialMC.Core.Asset (IsAsset)
import FinancialMC.Core.Flow (IsFlow)
import qualified FinancialMC.Core.MoneyValueOps as MV

import FinancialMC.Core.Engine (execOnePathPure,EngineC)

--import FinancialMC.Core.Flow (FlowDirection(..))
import FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..))
import FinancialMC.Core.MCState (HasMCState(..),PathSummary(..),CombinedState,HasCombinedState(..),netWorth,grossFlows)
import FinancialMC.Core.Utilities (Year)

import Control.Parallel.Strategies (using,parList,rseq)
import Statistics.Sample.Histogram (histogram)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Base  as VGB
import Control.Lens ((^.),(.~),(&))
import Data.Word (Word64)
import Control.Exception (SomeException)
import Data.List (foldl',sort,sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M

psToNumber::PathSummary->Double  
psToNumber (FinalNW mv) = mv ^. mAmount 
psToNumber (ZeroNW _) = 0

getHistory::CombinedState a fl le ru->[(Year,FSSummary)]
getHistory cs = cs ^. csMC.mcsHistory  

sortSummaries::[(PathSummary,a)]->[(PathSummary,a)]     
sortSummaries = sortBy (\(x,_) (y,_)-> compare (psToNumber x) (psToNumber y))

qIndices::Int->Int->[Int]
qIndices len quantiles = map (\n-> (2*n - 1)*len `div` (2 * quantiles)) [1..quantiles] 

qSubSet::[Int]->[a]->[a]
qSubSet is xs = map (\n -> xs !! n) is

historiesFromSummaries::EngineC a fl le ru rm=>
  LifeEventConverters a fl le->[(PathSummary,Word64)]->(FinEnv rm,CombinedState a fl le ru)->Bool->Int->Int->
  (Either SomeException) ([[(Year,MoneyValue)]],[(Year,FSSummary)])
historiesFromSummaries convertLE summaries (fe0,cs0) singleThreaded quantiles years = do
  let year0 = fe0 ^. feCurrentDate
      nw0 = netWorth cs0 fe0
      sorted = sortSummaries summaries
      (_,medianSeed) = sorted !! (length sorted `div` 2)
      csHist = cs0 & (csNeedHistory .~ True)
      getH seed = (getHistory . fst) <$> execOnePathPure convertLE csHist fe0 seed years 
      getNW::[(Year,FSSummary)]->[(Year,MoneyValue)]
      getNW x = (year0,nw0):map (\(d,FSSummary nw _ _ _ _ _)->(d,nw)) x
      inds = qIndices (length sorted) quantiles
      seeds = (snd. unzip) (qSubSet inds sorted)
  histories' <- sequence $ if singleThreaded then map getH seeds else map getH seeds `using` parList rseq
  let histories = map getNW histories'
      median0 = initialSummary cs0 fe0
  medianHist <- getH medianSeed
  return (histories,median0:medianHist)
  
  
nwHistFromSummaries::(VGB.Vector v1 Int,VGB.Vector v1 Double)=>[(PathSummary,Word64)]->Int->(v1 Double, v1 Int)
nwHistFromSummaries summaries bins = summariesToHistogram (sortSummaries summaries) bins


summariesToHistogram::(VGB.Vector v1 Double, VGB.Vector v1 Int)=>[(PathSummary,Word64)]->Int->(v1 Double, v1 Int)
summariesToHistogram summaries numBins = 
  let nws = V.fromList $ map (\(x,_)->psToNumber x) summaries
  in histogram numBins  nws 

initialSummary::(IsAsset a,IsFlow fl)=>CombinedState a fl le ru->FinEnv rm->(Year,FSSummary)                     
initialSummary cs0 fe0 =
  let nw =  netWorth cs0 fe0
      nwbo = netWorthBreakout cs0 fe0
      (inFlow,outFlow) = grossFlows (cs0 ^. (csMC.mcsCashFlows)) fe0
      in (fe0 ^. feCurrentDate,FSSummary nw nwbo inFlow outFlow (MV.zero (fe0 ^. feDefaultCCY)) 0) 

addReturns::[(Year,FSSummary)]->[(Year,FSSummary,MoneyValue,Double)]  
addReturns fs = result where 
  ccy = snd (head fs) ^. fssNW.mCurrency
  z = MV.zero ccy
  (dE,fsE) = last fs
  FSSummary nwF nwboF _ _ _ _ = fsE
  final = (dE, FSSummary nwF nwboF z z z 0, z, 0)
  l = zip fs (tail fs)
  f::FSSummary->FSSummary->(MoneyValue,Double)
  f (FSSummary nw0 _ _ _ _ _) (FSSummary nw1 _ in1 out1 tax1 _) = (retM,rate) where  
    nw0A = nw0 ^. mAmount
    nw1A = nw1 ^. mAmount
    inA = in1 ^. mAmount
    outA = out1 ^. mAmount
    tax1A = tax1 ^. mAmount
    returnA = (nw1A - nw0A) - (inA - outA) + tax1A
    rate = returnA / nw0A
    retM = MoneyValue returnA ccy
  g::((Year,FSSummary),(Year,FSSummary))->(Year,FSSummary,MoneyValue,Double)
  g ((d,fs0),(_,fs1)) = (d,fs',retM,rate) where
    (retM,rate) = f fs0 fs1
    FSSummary nw0 nwbo0 _ _ _ _ = fs0
    FSSummary _ _ in1 out1 tax1 tr1 = fs1
    fs' = FSSummary nw0 nwbo0 in1 out1 tax1 tr1
  result = map g l ++ [final] 


medianMode::[PathSummary]->(Maybe Year,Maybe Year)
medianMode sbs = (Just a, Just b) where
  (ZeroNW a) = sbs !! (length sbs `div` 2) -- NB: Not the median for even number. Returns earlier of central pair         
  countsM = foldl' (\m (ZeroNW d)->M.insertWith (+) d (1::Int) m) M.empty sbs 
  countsL = sortBy (comparing snd) $ M.toList countsM
  b = fst (last countsL)

analyzeBankruptcies::[PathSummary]->(Int,Maybe Year,Maybe Year)
analyzeBankruptcies bs = (num,median,mode) where
  sbs = sort bs
  num = length sbs
  (median,mode) = if num > 0 
                  then medianMode sbs
                  else (Nothing,Nothing)
  
