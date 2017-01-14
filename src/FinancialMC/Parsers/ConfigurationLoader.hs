{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module FinancialMC.Parsers.ConfigurationLoader
       (
         loadDataSources,
         getInitialStates,
         buildInitialStateFromConfig,
         buildInitialState,
         makeStartingRates,
         validateFinancialState,
         loadConfigurations
       ) where



import           FinancialMC.Base as Base
import           FinancialMC.Core.Asset (AccountName)
import           FinancialMC.Core.FinancialStates (FinEnv(..),zeroFinState,exchangeRFFromRateTable)
import           FinancialMC.Core.MCState (MCState,CombinedState(..),makeMCState,getAccountNames)
import           FinancialMC.Core.Rule (ruleAccounts)
import qualified FinancialMC.Core.MoneyValue as MV
import           FinancialMC.Core.Rates (Rate,RateTable,applyModel,defaultRateTable,RateModel)
import           FinancialMC.Core.Tax (FilingStatus,TaxRules)
import           FinancialMC.Core.Utilities (noteM,eitherToIO)
import qualified FinancialMC.Parsers.Configuration as C
import           FinancialMC.Parsers.JSON.BaseTypes (FMC_ParserMaps)
import           FinancialMC.Parsers.XML.ParseFinancialState (loadFinancialStatesFromString)
import qualified FinancialMC.Parsers.XML.ParseInput as XML
import           FinancialMC.Parsers.XML.ParseRateModel (loadRateModelsFromString)
import           FinancialMC.Parsers.XML.ParseTax (loadTaxDataFromString)


import qualified Control.Exception as E
import           Control.Monad (foldM)
import           Control.Monad.Catch (throwM,MonadThrow)
import           Control.Monad.State.Strict (StateT,execStateT)
import qualified Data.Map as M
import           Data.Random.Source.PureMT (pureMT)

import           Control.Lens (zoom,(%=),_1,_2)
import           Control.Monad.Morph (hoist,lift,generalize)
import           Data.Aeson.Existential.EnvParser (envEitherDecode,envEitherDecodeYaml)
import           Data.Aeson.Existential (EnvFromJSON)

loadDataSource::Maybe FilePath->FMC_ParserMaps->C.DataSource->StateT C.LoadedModels IO ()

loadDataSource mSchemaDir _ (C.DataSource (C.Parseable up C.XML) C.FinancialStateS) = 
  zoom C.lmFS $ lift (C.asIOString up) >>= loadFinancialStatesFromString mSchemaDir 
  -- validate!!

loadDataSource mSchemaDir _ (C.DataSource (C.Parseable up C.XML) C.TaxStructureS) =
  zoom C.lmTax $ lift (C.asIOString up) >>= loadTaxDataFromString mSchemaDir

loadDataSource mSchemaDir _ (C.DataSource (C.Parseable up C.XML) C.RateModelS) =
  zoom C.lmRM $ lift (C.asIOString up) >>= loadRateModelsFromString mSchemaDir

loadDataSource _ pMap (C.DataSource (C.Parseable up encoding) C.FinancialStateS) = do
  newStates <- lift $ decodeUnparsed pMap up encoding
  mapM_ (\(confName,x)-> lift $ eitherToIO $ validateFinancialState x confName) (M.toList newStates)
  C.lmFS %= M.union newStates

loadDataSource _ pMap (C.DataSource (C.Parseable up encoding) C.RateModelS) = do
  newRMs <- lift $ decodeUnparsed pMap up encoding 
  C.lmRM %= M.union newRMs

loadDataSource _ pMap (C.DataSource (C.Parseable up encoding) C.TaxStructureS) = do
  newTS <- lift $ decodeUnparsed pMap up encoding 
  zoom C.lmTax $ hoist generalize $ C.mergeTaxStructures newTS

loadDataSource _ _ (C.DataSource C.DBQuery _) = lift $ E.throwIO $ BadParse "DBQuery data source not yet implemented!"

loadDataSource _ _ _ = lift $ E.throwIO $ BadParse "Reached fall-through case in loadDataSource!"

decodeUnparsed::EnvFromJSON FMC_ParserMaps a=>FMC_ParserMaps->C.Unparsed->C.Encoding->IO a
decodeUnparsed _ _ C.XML = E.throwIO $ BadParse "XML handled specifically! Should not get to decodeUnparsed with an XML source." 

decodeUnparsed pMap up C.JSON = do
  lbs <- C.asIOLazyByteString up   
  case envEitherDecode pMap lbs of
    Left err -> E.throwIO $ BadParse ("While decoding (as JSON): " ++ show up ++ ": " ++ err)
    Right x -> return x

decodeUnparsed pMap up C.YAML = do
  bs <- C.asIOByteString up
  case envEitherDecodeYaml pMap bs of
    Left err -> E.throwIO $ BadParse ("While decoding (as YAML)" ++ show up ++ ": " ++ err)
    Right x -> return x

decodeUnparsed _ _ _ = E.throwIO $ BadParse "Fallthrough case in decodeUnparsed." 

loadDataSources::Maybe FilePath->FMC_ParserMaps->[C.DataSource]->StateT C.LoadedModels IO ()
loadDataSources mSchemaDir pMap = mapM_ (loadDataSource mSchemaDir pMap) 

loadXMLConfigs::Maybe FilePath->FMC_ParserMaps->C.Unparsed->StateT (C.LoadedModels,C.ModelDescriptionMap) IO () 
loadXMLConfigs mSchema pMap (C.UnparsedFile fp) = do
  (sources,configM) <- lift $ XML.loadConfigurations mSchema fp
  zoom _1 $ loadDataSources mSchema pMap sources
  hoist generalize $ _2 %= M.union configM 
loadXMLConfigs _ _ _ = lift $ E.throwIO $ BadParse "loadXMLConfigs called with Unparsed of sort other than UnparsedFile"

loadJSONConfigs::Maybe FilePath->FMC_ParserMaps->C.Unparsed->StateT (C.LoadedModels,C.ModelDescriptionMap) IO ()
loadJSONConfigs mSchema pMap up = do
  configLBS <- lift $ C.asIOLazyByteString up
  case envEitherDecode pMap configLBS of
    Left err -> lift $ E.throwIO $ BadParse err
    Right (C.ConfigurationInputs sources configM) -> do
      zoom _1 $ loadDataSources mSchema pMap sources
      hoist generalize $ _2 %= M.union configM 

loadYAMLConfigs::Maybe FilePath->FMC_ParserMaps->C.Unparsed->StateT (C.LoadedModels,C.ModelDescriptionMap) IO ()
loadYAMLConfigs mSchema pMap up = do
  configBS <- lift $ C.asIOByteString up
  case envEitherDecodeYaml pMap configBS of
    Left err -> lift $ E.throwIO $ BadParse err
    Right (C.ConfigurationInputs sources configM) -> do
      zoom _1 $ loadDataSources mSchema pMap sources
      hoist generalize $ _2 %= M.union configM 

loadConfigurationsS::Maybe FilePath->FMC_ParserMaps->C.Unparsed->StateT (C.LoadedModels,C.ModelDescriptionMap) IO ()
loadConfigurationsS mSchemaP pMaps up@(C.UnparsedFile configP) = loadC mSchemaP pMaps up where
  loadC = case C.encodingFromSuffix configP of 
    C.XML -> loadXMLConfigs 
    C.JSON -> loadJSONConfigs 
    C.YAML -> loadYAMLConfigs
    C.UnkEncoding -> \_ _ _ -> lift $ E.throwIO $ Other "Bad file type specified as config.  Only .xml .json and .yaml supported"

loadConfigurationsS _ _ _ = lift $ E.throwIO $ BadParse "loadConfigurationS called with Unparsed of sort other than UnparsedFile"


loadConfigurations::Maybe FilePath->FMC_ParserMaps->C.Unparsed->IO (C.LoadedModels,C.ModelDescriptionMap)
loadConfigurations mSchemaP pMaps up =
  execStateT (loadConfigurationsS mSchemaP pMaps up) (C.LoadedModels M.empty M.empty C.emptyTaxStructure,M.empty)

buildMCState::C.InitialFS->FinEnv->MCState
buildMCState (C.InitialFS bs cfs les rules sweepR taxTradeR) fe = makeMCState bs cfs fe les rules sweepR taxTradeR

makeStartingRates::MonadThrow m=>RateModel->m (RateTable Rate)
makeStartingRates rateDefaultModel = do
  (_,(startingRates,_)) <- applyModel (defaultRateTable,pureMT 1) rateDefaultModel --ICK.  Hard wired pureMT.  Ick.
  return startingRates

buildInitialState::C.InitialFS->TaxRules->RateTable Rate->RateModel->Year->MV.Currency->(FinEnv,CombinedState)
buildInitialState ifs taxRules startingRates rModel date ccy =
  let erF = exchangeRFFromRateTable startingRates
      fe = FinEnv startingRates erF date ccy taxRules rModel
      mcs = buildMCState ifs fe
      ics = CombinedState (zeroFinState ccy) mcs False
  in (fe, ics)

getInitialStates::C.LoadedModels->FilingStatus->(String,String,String,String,String,Maybe String)->Year->MV.Currency->Either E.SomeException (FinEnv,CombinedState)
getInitialStates (C.LoadedModels ifsM rmM ts) fstat (fsName,rdName,rmName,tfName,tsName,mtcName) date ccy = do
  initialFS <- 
    noteM (Other ("Couldn't find Financial State named \"" ++ fsName ++ "\"")) $ C.getFinancialState ifsM fsName 
  taxRules <- C.makeTaxRules ts fstat tfName tsName mtcName
  rateDefaultModel <- noteM (Other "Failed in getRateModel (defaults)") $ C.getRateModel rmM rdName 
  rateModel <- noteM (Other "Failed in getRateModel (model)") $ C.getRateModel rmM rmName
  startingRates  <- makeStartingRates rateDefaultModel
  return $ buildInitialState initialFS taxRules startingRates rateModel date ccy 


buildInitialStateFromConfig::C.LoadedModels->C.ModelDescriptionMap->String->Either E.SomeException (Maybe String,FinEnv,CombinedState)
buildInitialStateFromConfig ci cm confName = do 
  (C.ModelDescription fs op date ccy (rds,rms) (fstat,fed,st,cty)) <- 
    noteM (Other ("Failed to find conf=" ++ confName)) $ M.lookup confName cm
  (fe,cs) <- getInitialStates ci fstat (fs,rds,rms,fed,st,cty) date ccy
  return (op,fe,cs)


exchangeRates::MV.ExchangeRateFunction
exchangeRates MV.USD MV.EUR = 0.8
exchangeRates MV.EUR MV.USD = 1.0/exchangeRates MV.USD MV.EUR
exchangeRates _ _     = 1.0


validateFinancialState::C.InitialFS->String->Either E.SomeException ()
validateFinancialState (C.InitialFS bs _ _ rs sw tt) configName = do
  let validAccountNames = getAccountNames bs
      checkName::AccountName->Either E.SomeException ()
      checkName name = if name `elem` validAccountNames
                       then Right () 
                       else throwM (Other ("(" ++ configName ++ ") Account \"" ++ show name ++ "\" required by a rule but not found."))
      checkRule rule = foldM (\_ name-> checkName name) () (ruleAccounts rule)
      allRules = rs ++ [sw,tt]
  foldM (\_ r->checkRule r) () allRules




