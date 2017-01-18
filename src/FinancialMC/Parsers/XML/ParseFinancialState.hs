{-# LANGUAGE Arrows, NoMonomorphismRestriction, FlexibleContexts #-}
module FinancialMC.Parsers.XML.ParseFinancialState 
       (
         loadFinancialStatesFromFile,
         loadFinancialStatesFromString,
         InitialFS(..),
         IFSMap
       ) where


import FinancialMC.Parsers.Configuration (InitialFS(..),IFSMap)
import FinancialMC.Parsers.XML.Utilities (buildOpts,XmlParseInfos,runFMCX,FMCXmlArrow,atTag)
import FinancialMC.Parsers.XML.Account (getBalanceSheet)
import FinancialMC.Parsers.XML.Flow (getCashFlows)
import FinancialMC.Parsers.XML.LifeEvent (getLifeEvents)
import FinancialMC.Parsers.XML.Rule (getRules,getSweepRule,getTaxTradeRule)

import FinancialMC.Builders.Assets (FMCBaseAsset)
import FinancialMC.Builders.LifeEvents (BaseLifeEvent)

import Text.XML.HXT.Core (withRemoveWS,yes,readString,IOSLA,XIOState,XmlTree,(>>>),getAttrValue,returnA)
import Control.Monad.State.Strict (StateT,lift,MonadTrans,MonadState,get,put)


import qualified Data.Map as M



loadFinancialStatesFromFile::Maybe FilePath->FilePath->StateT (IFSMap FMCBaseAsset BaseLifeEvent) IO ()
loadFinancialStatesFromFile mSchemaDir file = 
  lift (readFile file) >>= loadFinancialStatesFromString mSchemaDir

loadFinancialStatesFromString::Maybe FilePath->String->StateT (IFSMap FMCBaseAsset BaseLifeEvent) IO () 
loadFinancialStatesFromString mSchemaDir content = do
  let opts = buildOpts mSchemaDir [withRemoveWS yes] "FinancialStates.rng"
  let xml = readString opts content
  loadFinancialStates' xml
  

loadFinancialStates'::(MonadTrans t, MonadState (IFSMap FMCBaseAsset BaseLifeEvent) (t IO)) =>
                      IOSLA (XIOState XmlParseInfos) XmlTree XmlTree -> t IO ()
loadFinancialStates' xml = do  
  pfm <- get
  pfs <- lift $ runFMCX (xml >>> getPersonalFinances)
--  mapM_ (\(confName,x)-> lift $ eitherToIO $ validateFinancialState x confName) pfs
  let f (name,ifs) = M.insert name ifs  
      result = foldr f pfm pfs
  put result

    
getPersonalFinances::FMCXmlArrow XmlTree (String,InitialFS FMCBaseAsset BaseLifeEvent)                 
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
    
