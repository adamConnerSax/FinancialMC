{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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



import           FinancialMC.Base                            as Base
import           FinancialMC.Core.Asset                      (AccountName)
import           FinancialMC.Core.FinancialStates            (FinEnv (..), exchangeRFFromRateTable,
                                                              zeroFinState)
import           FinancialMC.Core.MCState                    (CombinedState (..),
                                                              MCState,
                                                              getAccountNames,
                                                              makeMCState)
import qualified FinancialMC.Core.MoneyValue                 as MV
import           FinancialMC.Core.Rates                      (Rate, RateModel,
                                                              RateTable,
                                                              applyModel,
                                                              defaultRateTable)
import           FinancialMC.Core.Rule                       (IsRule,
                                                              ruleAccounts)
import           FinancialMC.Core.Tax                        (FilingStatus,
                                                              TaxRules)
import           FinancialMC.Core.Utilities                  (eitherToIO, noteM)
import qualified FinancialMC.Parsers.Configuration           as C
import           FinancialMC.Parsers.JSON.BaseTypes          (FMC_ParserMaps)
import           FinancialMC.Parsers.XML.ParseFinancialState (loadFinancialStatesFromString)
import qualified FinancialMC.Parsers.XML.ParseInput          as XML
import           FinancialMC.Parsers.XML.ParseRateModel      (loadRateModelsFromString)
import           FinancialMC.Parsers.XML.ParseTax            (loadTaxDataFromString)

import           FinancialMC.Base                            (BaseAsset,
                                                              BaseFlow,
                                                              BaseLifeEvent,
                                                              BaseRule)

import qualified Control.Exception                           as E
import           Control.Monad                               (foldM)
import           Control.Monad.Catch                         (MonadThrow,
                                                              throwM)
import           Control.Monad.State.Strict                  (StateT,
                                                              execStateT)
import qualified Data.Map                                    as M
import           Data.Random.Source.PureMT                   (pureMT)

import           Control.Lens                                (zoom, (%=), _1,
                                                              _2)
import           Control.Monad.Morph                         (generalize, hoist,
                                                              lift)
import           Data.Aeson                                  (FromJSON)
import           Data.Aeson.Existential                      (EnvFromJSON (..))
import           Data.Aeson.Existential.EnvParser            (envEitherDecode, envEitherDecodeYaml)

type BaseFCC a fl le ru = C.FMCComponentConverters BaseAsset a BaseFlow fl BaseLifeEvent le BaseRule ru
type FJ a fl le ru = (FromJSON a,
                      FromJSON fl, EnvFromJSON FMC_ParserMaps fl,
                      FromJSON le, EnvFromJSON FMC_ParserMaps le,
                      FromJSON ru, EnvFromJSON FMC_ParserMaps ru)

loadDataSource::(FJ a fl le ru,IsRule ru)=>BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->
                C.DataSource->StateT (C.LoadedModels a fl le ru) IO ()
loadDataSource fcc mSchemaDir _ (C.DataSource (C.Parseable up C.XML) C.FinancialStateS) = do
  let newFS = execState
  zoom C.lmFS $ lift (C.asIOString up) >>= loadFinancialStatesFromString fcc mSchemaDir
  -- validate!!

loadDataSource _ mSchemaDir _ (C.DataSource (C.Parseable up C.XML) C.TaxStructureS) =
  zoom C.lmTax $ lift (C.asIOString up) >>= loadTaxDataFromString mSchemaDir

loadDataSource _ mSchemaDir _ (C.DataSource (C.Parseable up C.XML) C.RateModelS) =
  zoom C.lmRM $ lift (C.asIOString up) >>= loadRateModelsFromString mSchemaDir

loadDataSource _ _ pMap (C.DataSource (C.Parseable up encoding) C.FinancialStateS) = do
  newStates <- lift $ decodeUnparsed pMap up encoding
  mapM_ (\(confName,x)-> lift $ eitherToIO $ validateFinancialState x confName) (M.toList newStates)
  C.lmFS %= M.union newStates

loadDataSource _ _ pMap (C.DataSource (C.Parseable up encoding) C.RateModelS) = do
  newRMs <- lift $ decodeUnparsed pMap up encoding
  C.lmRM %= M.union newRMs

loadDataSource _ _ pMap (C.DataSource (C.Parseable up encoding) C.TaxStructureS) = do
  newTS <- lift $ decodeUnparsed pMap up encoding
  zoom C.lmTax $ hoist generalize $ C.mergeTaxStructures newTS

loadDataSource _ _ _ (C.DataSource C.DBQuery _) = lift $ E.throwIO $ BadParse "DBQuery data source not yet implemented!"

loadDataSource _ _ _ _ = lift $ E.throwIO $ BadParse "Reached fall-through case in loadDataSource!"

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

loadDataSources::(FJ a fl le ru,IsRule ru)=>
  BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->[C.DataSource]->StateT (C.LoadedModels a fl le ru) IO ()
loadDataSources fcc mSchemaDir pMap = mapM_ (loadDataSource fcc mSchemaDir pMap)

loadXMLConfigs::(FJ a fl le ru,IsRule ru)=>
  BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->C.Unparsed->StateT (C.LoadedModels a fl le ru,C.ModelDescriptionMap) IO ()
loadXMLConfigs fcc mSchema pMap (C.UnparsedFile fp) = do
  (sources,configM) <- lift $ XML.loadConfigurations mSchema fp
  zoom _1 $ loadDataSources fcc mSchema pMap sources
  hoist generalize $ _2 %= M.union configM
loadXMLConfigs _ _ _ _ = lift $ E.throwIO $ BadParse "loadXMLConfigs called with Unparsed of sort other than UnparsedFile"

loadJSONConfigs::(FJ a fl le ru,IsRule ru)=>
  BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->C.Unparsed->
  StateT (C.LoadedModels a fl le ru,C.ModelDescriptionMap) IO ()
loadJSONConfigs fcc mSchema pMap up = do
  configLBS <- lift $ C.asIOLazyByteString up
  case envEitherDecode pMap configLBS of
    Left err -> lift $ E.throwIO $ BadParse err
    Right (C.ConfigurationInputs sources configM) -> do
      zoom _1 $ loadDataSources fcc mSchema pMap sources
      hoist generalize $ _2 %= M.union configM

loadYAMLConfigs::(FJ a fl le ru,IsRule ru)=>
  BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->C.Unparsed->
  StateT (C.LoadedModels a fl le ru,C.ModelDescriptionMap) IO ()
loadYAMLConfigs fcc mSchema pMap up = do
  configBS <- lift $ C.asIOByteString up
  case envEitherDecodeYaml pMap configBS of
    Left err -> lift $ E.throwIO $ BadParse err
    Right (C.ConfigurationInputs sources configM) -> do
      zoom _1 $ loadDataSources fcc mSchema pMap sources
      hoist generalize $ _2 %= M.union configM

loadConfigurationsS::(FJ a fl le ru,IsRule ru)=>
  BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->C.Unparsed->
  StateT (C.LoadedModels a fl le ru,C.ModelDescriptionMap) IO ()
loadConfigurationsS fcc mSchemaP pMaps up@(C.UnparsedFile configP) = loadC fcc mSchemaP pMaps up where
  loadC = case C.encodingFromSuffix configP of
    C.XML -> loadXMLConfigs
    C.JSON -> loadJSONConfigs
    C.YAML -> loadYAMLConfigs
    C.UnkEncoding -> \_ _ _ _ -> lift $ E.throwIO $ Other "Bad file type specified as config.  Only .xml .json and .yaml supported"

loadConfigurationsS _ _ _ _ = lift $ E.throwIO $ BadParse "loadConfigurationS called with Unparsed of sort other than UnparsedFile"


loadConfigurations::(FJ a fl le ru,IsRule ru)=>
  BaseFCC a fl le ru->Maybe FilePath->FMC_ParserMaps->C.Unparsed->IO (C.LoadedModels a fl le ru,C.ModelDescriptionMap)
loadConfigurations fcc mSchemaP pMaps up =
  execStateT (loadConfigurationsS fcc mSchemaP pMaps up) (C.LoadedModels M.empty M.empty C.emptyTaxStructure,M.empty)

buildMCState::C.InitialFS a fl le ru->FinEnv->MCState a fl le ru
buildMCState (C.InitialFS bs cfs les rules sweepR taxTradeR) fe = makeMCState bs cfs fe les rules sweepR taxTradeR

makeStartingRates::MonadThrow m=>RateModel->m (RateTable Rate)
makeStartingRates rateDefaultModel = do
  (_,(startingRates,_)) <- applyModel (defaultRateTable,pureMT 1) rateDefaultModel --ICK.  Hard wired pureMT.  Ick.
  return startingRates

buildInitialState::C.InitialFS a fl le ru->TaxRules->RateTable Rate->RateModel->Year->MV.Currency->(FinEnv,CombinedState a fl le ru)
buildInitialState ifs taxRules startingRates rModel date ccy =
  let erF = exchangeRFFromRateTable startingRates
      fe = FinEnv startingRates erF date ccy taxRules rModel
      mcs = buildMCState ifs fe
      ics = CombinedState (zeroFinState ccy) mcs False
  in (fe, ics)

getInitialStates::C.LoadedModels a fl le ru->FilingStatus->(String,String,String,String,String,Maybe String)->Year->MV.Currency->Either E.SomeException (FinEnv,CombinedState a fl le ru)
getInitialStates (C.LoadedModels ifsM rmM ts) fstat (fsName,rdName,rmName,tfName,tsName,mtcName) date ccy = do
  initialFS <-
    noteM (Other ("Couldn't find Financial State named \"" ++ fsName ++ "\"")) $ C.getFinancialState ifsM fsName
  taxRules <- C.makeTaxRules ts fstat tfName tsName mtcName
  rateDefaultModel <- noteM (Other "Failed in getRateModel (defaults)") $ C.getRateModel rmM rdName
  rateModel <- noteM (Other "Failed in getRateModel (model)") $ C.getRateModel rmM rmName
  startingRates  <- makeStartingRates rateDefaultModel
  return $ buildInitialState initialFS taxRules startingRates rateModel date ccy


buildInitialStateFromConfig::C.LoadedModels a fl le ru->C.ModelDescriptionMap->String->Either E.SomeException (Maybe String,FinEnv,CombinedState a fl le ru)
buildInitialStateFromConfig ci cm confName = do
  (C.ModelDescription fs op date ccy (rds,rms) (fstat,fed,st,cty)) <-
    noteM (Other ("Failed to find conf=" ++ confName)) $ M.lookup confName cm
  (fe,cs) <- getInitialStates ci fstat (fs,rds,rms,fed,st,cty) date ccy
  return (op,fe,cs)


exchangeRates::MV.ExchangeRateFunction
exchangeRates MV.USD MV.EUR = 0.8
exchangeRates MV.EUR MV.USD = 1.0/exchangeRates MV.USD MV.EUR
exchangeRates _ _     = 1.0


validateFinancialState::IsRule ru=>C.InitialFS a fl le ru->String->Either E.SomeException ()
validateFinancialState (C.InitialFS bs _ _ rs sw tt) configName = do
  let validAccountNames = getAccountNames bs
      checkName::AccountName->Either E.SomeException ()
      checkName name = if name `elem` validAccountNames
                       then Right ()
                       else throwM (Other ("(" ++ configName ++ ") Account \"" ++ show name ++ "\" required by a rule but not found."))
      checkRule rule = foldM (\_ name-> checkName name) () (ruleAccounts rule)
      allRules = rs ++ [sw,tt]
  foldM (\_ r->checkRule r) () allRules




