{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module FinancialMC.Parsers.XML.ParseFinancialState 
       (
         loadFinancialStatesFromFile,
         loadFinancialStatesFromString,
         InitialFS(..),
         IFSMap
       ) where


import FinancialMC.Parsers.Configuration (InitialFS(..),IFSMap,FMCComponentConverters(..),FMCConvertible(..))
import FinancialMC.Parsers.XML.Utilities (buildOpts,XmlParseInfos,runFMCX,FMCXmlArrow,atTag)
import FinancialMC.Parsers.XML.Account (getBalanceSheet)
import FinancialMC.Parsers.XML.Flow (getCashFlows)
import FinancialMC.Parsers.XML.LifeEvent (getLifeEvents)
import FinancialMC.Parsers.XML.Rule (getRules,getSweepRule,getTaxTradeRule)

import FinancialMC.Builders.Assets (BaseAsset)
import FinancialMC.Builders.LifeEvents (BaseLifeEvent)
import FinancialMC.Builders.Flows (BaseFlow)
import FinancialMC.Builders.Rules (BaseRule)

import Text.XML.HXT.Core (withRemoveWS,yes,readString,IOSLA,XIOState,XmlTree,(>>>),getAttrValue,returnA)
import Control.Monad.State.Strict (StateT,lift,MonadTrans,MonadState,get,put)


import qualified Data.Map as M

type FCC a fl le ru = FMCComponentConverters BaseAsset a BaseFlow fl BaseLifeEvent le BaseRule ru

loadFinancialStatesFromFile::FCC a fl le ru->Maybe FilePath->FilePath->StateT (IFSMap a fl le ru) IO ()
loadFinancialStatesFromFile fcc mSchemaDir file = 
  lift (readFile file) >>= loadFinancialStatesFromString fcc mSchemaDir

loadFinancialStatesFromString::FCC a fl le ru->Maybe FilePath->String->StateT (IFSMap a fl le ru) IO () 
loadFinancialStatesFromString fcc mSchemaDir content = do
  let opts = buildOpts mSchemaDir [withRemoveWS yes] "FinancialStates.rng"
  let xml = readString opts content
  loadFinancialStates' fcc xml
  

loadFinancialStates'::(MonadTrans t, MonadState (IFSMap a fl le ru) (t IO)) =>
                      FCC a fl le ru -> IOSLA (XIOState XmlParseInfos) XmlTree XmlTree -> t IO ()
loadFinancialStates' fcc xml = do  
  pfm <- get
  pfs <- lift $ runFMCX (xml >>> getPersonalFinances)
--  mapM_ (\(confName,x)-> lift $ eitherToIO $ validateFinancialState x confName) pfs
  let f (name,ifs) = M.insert name (fmcMap fcc ifs)  
      result = foldr f pfm pfs
  put result

    
getPersonalFinances::FMCXmlArrow XmlTree (String,InitialFS BaseAsset BaseFlow BaseLifeEvent BaseRule)                 
getPersonalFinances = atTag "PersonalFinances" >>>
  proc l -> do
    name <- getAttrValue "name" -< l
    balanceSheet <- getBalanceSheet -< l
    cashFlows <- getCashFlows -< l
    lifeEvents <- getLifeEvents -< l
    sRule <- getSweepRule -< l
    ttRule <- getTaxTradeRule -< l
    rules <- getRules -< l
    returnA -< (name,InitialFS balanceSheet cashFlows lifeEvents rules sRule ttRule)
    
