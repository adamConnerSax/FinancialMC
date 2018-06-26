{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.ParseInput
       (
         loadConfigurations
       , getConfiguration
       , getXMLDataSources,
       ) where


import           FinancialMC.Core.MoneyValue                 (Currency (USD))

import           FinancialMC.Core.Tax                        (FilingStatus (..))

import qualified FinancialMC.Parsers.Configuration           as C
import           FinancialMC.Parsers.XML.ParseFinancialState (BaseTag, loadFinancialStatesFromFile)
import           FinancialMC.Parsers.XML.ParseRateModel      (loadRateModelsFromFile)
import           FinancialMC.Parsers.XML.ParseTax            (emptyTaxStructure, loadTaxDataFromFile)

{-
import           FinancialMC.Builders.Assets                 (BaseAsset)
import           FinancialMC.Builders.Flows                  (BaseFlow)
import           FinancialMC.Builders.LifeEvents             (BaseLifeEvent)
import           FinancialMC.Builders.Rules                  (BaseRule)
-}
import           FinancialMC.Builders.RateModels             (BaseRateModelT)


import           FinancialMC.Parsers.XML.Utilities           (FMCXmlArrow,
                                                              atTag,
                                                              getAttrValueIf,
                                                              parseXML,
                                                              readAttrValueDef,
                                                              runFMCX)

import           Control.Monad.State.Strict                  (StateT,
                                                              execStateT)
import           Text.XML.HXT.Core                           (XmlTree,
                                                              getAttrValue,
                                                              getChildren,
                                                              getText, listA,
                                                              returnA, returnA,
                                                              (>>>))

import qualified Data.Map                                    as M

getXMLDataSources::FMCXmlArrow XmlTree ([String],[String],[String])
getXMLDataSources = atTag "DataSources" >>>
  proc l -> do
    td <- atTag "TaxData" -< l
    taxXMLs <- listA (atTag "XML" >>> getChildren >>> getText) -< td
    rm <- atTag "RateModels" -< l
    rateXMLs <- listA (atTag "XML" >>> getChildren >>> getText) -< rm
    fs <- atTag "FinancialStates" -< l
    finStateXMLs <- listA (atTag "XML" >>> getChildren >>> getText) -< fs
    returnA -< (taxXMLs,rateXMLs,finStateXMLs)


getConfiguration::FMCXmlArrow XmlTree (String,C.ModelDescription)
getConfiguration = atTag "Configuration" >>>
  proc l -> do
    name <- getAttrValue "name" -< l
    finState <- getAttrValue "financial_state" -< l
    outputPrefix <- getAttrValueIf "output_prefix" -< l
    curDate <- readAttrValueDef "current_date" 2015 -< l
    ccy <- readAttrValueDef "currency" USD -< l
    rates <- atTag "RateSetup" -< l
    rateDefaults <- getAttrValue "defaults" -< rates
    rateModel <- getAttrValue "model" -< rates
    taxes <- atTag "TaxSetup" -< l
    filingStatus <- readAttrValueDef "filing_status" MarriedFilingJointly -< taxes
    fed <- getAttrValue "federal" -< taxes
    st <- getAttrValue "state" -< taxes
    city <- getAttrValueIf "city" -< taxes
    returnA -< (name,C.ModelDescription finState outputPrefix curDate ccy (rateDefaults,rateModel) (filingStatus,fed,st,city))


getConfigurations::FMCXmlArrow XmlTree C.ModelDescriptionMap
getConfigurations = atTag "Configurations" >>>
  proc l -> do
    configs <- listA getConfiguration -< l
    returnA -< M.fromList configs


--type FCC a fl le ru rm = C.FMCComponentConverters BaseAsset a BaseFlow fl BaseLifeEvent le BaseRule ru BaseRateModelT rm

loadConfigurations'::Maybe String->FilePath->IO (C.LoadedModels BaseTag BaseRateModelT, C.ModelDescriptionMap)
loadConfigurations' mSchema path = do
  let configXML = parseXML path
  result <- runFMCX (configXML >>> getXMLDataSources)
  let (taxXMLs,rateXMLs,finStateXMLs) = head result
      f::[String]->(String->StateT a IO ())->a->IO a
      f list load = execStateT (mapM_ load list)
  taxStructure <- f taxXMLs (loadTaxDataFromFile mSchema) emptyTaxStructure
  rateModels <-   f rateXMLs (loadRateModelsFromFile mSchema) M.empty
  finStates <-    f finStateXMLs (loadFinancialStatesFromFile mSchema) M.empty
  configs <- runFMCX (configXML >>> getConfigurations)
  return ((C.LoadedModels finStates rateModels taxStructure),head configs)

loadConfigurations::Maybe String->FilePath->IO ([C.DataSource],C.ModelDescriptionMap)
loadConfigurations mSchema path = do
  let configXML = parseXML path
  result <- runFMCX (configXML >>> getXMLDataSources)
  let (taxXMLs,rateXMLs,finStateXMLs) = head result
      toXMLDS contentType xmlPath = C.DataSource (C.Parseable (C.UnparsedFile xmlPath) C.XML) contentType
      taxDS = (toXMLDS C.TaxStructureS) <$> taxXMLs
      rateDS = (toXMLDS C.RateModelS) <$> rateXMLs
      fsDS = (toXMLDS C.FinancialStateS) <$> finStateXMLs
  configs <- runFMCX (configXML >>> getConfigurations)
  return (taxDS ++ rateDS ++ fsDS,head configs)


{--

getConfigurations::ArrowXml a=>a XmlTree (M.Map String (FinEnv,CombinedState))
getConfiguration = proc l




test::IO ()
main = do
  configMap <- loadFinancialStates "Config.xml")
  print configMap

--}
