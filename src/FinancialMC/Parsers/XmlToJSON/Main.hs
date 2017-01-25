{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State.Strict (StateT,execStateT,get,put,lift,MonadTrans,MonadState)
import qualified Data.Map as M
import Data.Monoid ((<>))

import           Text.XML.HXT.Core (IOSLA,XIOState,XmlTree,(>>>),withRemoveWS,yes,readDocument)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Yaml as Y
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)

import           Options.Applicative (Parser,optional,long,metavar,str,argument,execParser,ParserInfo,
                                      header,strOption,ReadM,readerError,option,short,help,info,helper,fullDesc,progDesc)

import qualified FinancialMC.Parsers.Configuration as C

import           FinancialMC.Parsers.XML.All (loadFinancialStatesFromFile,
                                              getConfiguration,getXMLDataSources,
                                              loadRateModelsFromFile,
                                              loadTaxDataFromFile,emptyTaxStructure)
import           FinancialMC.Parsers.XML.Utilities (XmlParseInfos,runFMCX,buildOpts)

--import           FinancialMC.Base (BaseAsset,BaseFlow,BaseLifeEvent,BaseRule,BaseRateModelT)


data XmlType = FinStates | RModels | TData | Configs deriving (Show,Enum,Bounded,Ord,Eq)
data OutputType = YAML | JSON deriving (Show,Enum,Bounded,Ord,Eq)

outputTypeToEncoder::A.ToJSON a=>OutputType->a->B.ByteString
outputTypeToEncoder YAML = Y.encode
outputTypeToEncoder JSON = B.concat . LB.toChunks . encodePretty

data XmlToJSONOptions = XmlToJSONOptions
     { optSchemaDir :: Maybe String,
       optFileType :: XmlType,
       optOutputType :: OutputType,
       optFileToConvert :: String }


parseBoundedEnum::(Enum e, Bounded e, Show e) => String -> ReadM e
parseBoundedEnum s = do
  let enumValues = [minBound..]  
      valueMap = (\x -> (show x,x)) <$> enumValues
      val = lookup s valueMap
      r (Nothing) = readerError ("Couldn't parse \"" ++ s ++ "\" as a member of " ++ show enumValues)  
      r (Just x) = return x 
  r val

parseXmlType::Parser XmlType
parseXmlType = option (str >>= parseBoundedEnum) (long "xmlType"
                                                  <> short 't'
                                                  <> metavar "XML_TYPE"
                                                  <> help "Parse as XML_TYPE (FinStates,RModels,TData,Configs) file")
               
parseOutputType::Parser OutputType
parseOutputType = option (str >>= parseBoundedEnum) (long "outputType"
                                                     <> short 'o'
                                                     <> metavar "OUTPUT_TYPE"
                                                     <> help "output as OUTPUT_TYPE (YAML,JSON)")
                  
                  
xmlToJSONOptionParser'::Parser XmlToJSONOptions
xmlToJSONOptionParser' = XmlToJSONOptions
                       <$> (optional $ strOption (long "schemaDir"
                                                  <> metavar "SCHEMADIR"
                                                  <> help "load rng schema from SCHEMADIR"))                    
                       <*> parseXmlType
                       <*> parseOutputType
                       <*> argument str (metavar "Files to convert")

xmlToJSONOptionParser::ParserInfo XmlToJSONOptions
xmlToJSONOptionParser = info (helper <*> xmlToJSONOptionParser')
                        (fullDesc
                         <> progDesc "convert FinancialMC XML to JSON (or YAML or TOML?)"
                         <> header "XML to JSON converter for FinancialMC XML")


xmlSourcesToDS::([String],[String],[String]) -> [C.DataSource]
xmlSourcesToDS (taxL,rateL,finL) = taxDS ++ rateDS ++ finDS where
  taxDS = (\fn -> C.DataSource (C.Parseable (C.UnparsedFile fn) C.XML) C.TaxStructureS) <$> taxL
  rateDS = (\fn -> C.DataSource (C.Parseable (C.UnparsedFile fn) C.XML) C.RateModelS) <$> rateL
  finDS = (\fn -> C.DataSource (C.Parseable (C.UnparsedFile fn) C.XML) C.FinancialStateS) <$> finL

-- This one isn't present in main code since we never load just a set of configs
loadConfigurations'::(MonadTrans t, MonadState C.ConfigurationInputs (t IO))=>
                     IOSLA (XIOState XmlParseInfos) XmlTree XmlTree->t IO ()
loadConfigurations' xml = do
  (C.ConfigurationInputs sources oldMap) <- get
  xmlSources <- lift $ runFMCX ( xml >>> getXMLDataSources)
  newConfigMap <- lift $ runFMCX (xml >>> getConfiguration)
  let f (name,config) = M.insert name config
      newMap = foldr f oldMap newConfigMap
  put (C.ConfigurationInputs (sources ++ xmlSourcesToDS (head xmlSources)) newMap)

loadConfigurationsFromFile::Maybe String->String->StateT C.ConfigurationInputs IO ()
loadConfigurationsFromFile mSchemaDir file = do
  let opts = buildOpts mSchemaDir [withRemoveWS yes] "FinancialStates.rng"
  let xml = readDocument opts file
  loadConfigurations' xml
--  

--ccs::C.FMCComponentConverters BaseAsset BaseAsset BaseFlow BaseFlow BaseLifeEvent BaseLifeEvent BaseRule BaseRule BaseRateModelT BaseRateModelT
--ccs = C.FMCComponentConverters id id id id id

main::IO ()
main = do
     options <- execParser xmlToJSONOptionParser
     let ot = optOutputType options
     let parseXml::Maybe String->String->IO ()
         parseXml mSchemaDir fileName = do
           let fileType = optFileType options
           jsonPretty <- case fileType of
             FinStates -> outputTypeToEncoder ot <$> execStateT (loadFinancialStatesFromFile mSchemaDir fileName) M.empty 
             RModels -> outputTypeToEncoder ot <$> execStateT (loadRateModelsFromFile mSchemaDir fileName) M.empty 
             TData -> outputTypeToEncoder ot <$> execStateT (loadTaxDataFromFile mSchemaDir fileName) emptyTaxStructure
             Configs -> outputTypeToEncoder ot <$> execStateT (loadConfigurationsFromFile mSchemaDir fileName) (C.ConfigurationInputs [] M.empty)
           B.putStr jsonPretty
     parseXml (optSchemaDir options) (optFileToConvert options)                    
                                           








