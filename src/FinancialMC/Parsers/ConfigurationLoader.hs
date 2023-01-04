{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
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
import           FinancialMC.Core.FinancialStates            (FinEnv (..), exchangeMatrixFromRateTable,
                                                              zeroFinState)
import           FinancialMC.Core.MCState                    (CombinedState (..),
                                                              ComponentTypes (..),
                                                              FromJSONComponents,
                                                              MCState,
                                                              ShowableComponents (..),
                                                              getAccountNames,
                                                              makeMCState)
import qualified FinancialMC.Core.MoneyValue                 as MV
import           FinancialMC.Core.Rates                      (IsRateModel, Rate,
                                                              RateTable,
                                                              defaultArrayBasedRateTable,
                                                              runModel)
import           FinancialMC.Core.Rule                       (IsRule,
                                                              ruleAccounts)
import           FinancialMC.Core.Tax                        (FilingStatus,
                                                              TaxRules)
import           FinancialMC.Core.Utilities                  (AsFMCException (..),
                                                              FMCException,
                                                              eitherToIO, noteM)
import qualified FinancialMC.Parsers.Configuration           as C
import           FinancialMC.Parsers.XML.ParseFinancialState (loadFinancialStatesFromString)
import qualified FinancialMC.Parsers.XML.ParseInput          as XML
import           FinancialMC.Parsers.XML.ParseRateModel      (loadRateModelsFromString)
import           FinancialMC.Parsers.XML.ParseTax            (loadTaxDataFromString)

import qualified Control.Exception                           as E
import           Control.Exception.Lens                      (throwingM)
import           Control.Monad                               (foldM)
import           Control.Monad.State.Strict                  (StateT,
                                                              execStateT, get,
                                                              put)
import qualified Data.Map                                    as M
import           System.Random.Mersenne.Pure64  (pureMT)
import           FinancialMC.Base                            (BaseAsset,
                                                              BaseFlow,
                                                              BaseLifeEvent,
                                                              BaseRateModelT,
                                                              BaseRule)
import           FinancialMC.BaseComponents                  (BaseComponents)

import           Control.Lens                                (zoom, (%=), _1,
                                                              _2)
import           Control.Monad.Error.Lens                    (throwing)
--import           Control.Monad.Except                        (MonadError)
import           Control.Monad.Morph                         (generalize, hoist,
                                                              lift)
import           Data.Aeson                                  (FromJSON,
                                                              eitherDecode)
import           Data.Monoid                                 ((<>))
import qualified Data.Text                                   as T
import qualified Data.Yaml                                   as YAML

type BaseFCC tag = C.FMCComponentConverters BaseComponents tag
type FJ tag = (FromJSONComponents tag)

loadDataSource :: FJ tag
               => BaseFCC tag
               -> Maybe FilePath
               -> C.DataSource
               -> StateT (C.LoadedModels tag) IO ()
loadDataSource fcc mSchemaDir (C.DataSource (C.Parseable up C.XML) C.FinancialStateS) = do
  zoom C.lmFS $ do
    currentFS <- get
    newFS <- lift $ do
      xmlS <- (C.asIOString up)
      C.convertComponentsIFSMap fcc <$> execStateT (loadFinancialStatesFromString mSchemaDir xmlS) M.empty
    put $ M.union currentFS newFS

{-  zoom C.lmFS $ lift (C.asIOString up) >>= loadFinancialStatesFromString fcc mSchemaDir -}
  -- validate!!
loadDataSource _ mSchemaDir (C.DataSource (C.Parseable up C.XML) C.TaxStructureS) =
  zoom C.lmTax $ lift (C.asIOString up) >>= loadTaxDataFromString mSchemaDir

loadDataSource (C.FMCComponentConverters _ _ _ _ rmF) mSchemaDir (C.DataSource (C.Parseable up C.XML) C.RateModelS) =
  zoom C.lmRM $ do
    currentRM <- get
    newRM <- lift $ do
      xmlS <- (C.asIOString up)
      fmap rmF <$> execStateT (loadRateModelsFromString mSchemaDir xmlS) M.empty
    put $ M.union currentRM newRM

loadDataSource _ _ (C.DataSource (C.Parseable up encoding) C.FinancialStateS) = do
  newStates <- lift $ decodeUnparsed up encoding
  mapM_ (\(confName,x)-> lift . eitherToIO $ validateFinancialState x confName) (M.toList newStates)
  C.lmFS %= M.union newStates

loadDataSource _ _ (C.DataSource (C.Parseable up encoding) C.RateModelS) = do
  newRMs <- lift $ decodeUnparsed up encoding
  C.lmRM %= M.union newRMs

loadDataSource _ _ (C.DataSource (C.Parseable up encoding) C.TaxStructureS) = do
  newTS <- lift $ decodeUnparsed up encoding
  zoom C.lmTax . hoist generalize $ C.mergeTaxStructures newTS

loadDataSource _ _ (C.DataSource C.DBQuery _) = lift $ throwingM _BadParse "DBQuery data source not yet implemented!"

--loadDataSource _ _ _ = lift . E.throwIO $ BadParse "Reached fall-through case in loadDataSource!"

decodeUnparsed::FromJSON a=>C.Unparsed->C.Encoding->IO a
decodeUnparsed _ C.XML = E.throwIO $ BadParse "XML handled specifically! Should not get to decodeUnparsed with an XML source."

decodeUnparsed up C.JSON = do
  lbs <- C.asIOLazyByteString up
  case eitherDecode lbs of
    Left err -> throwingM _BadParse ("While decoding (as JSON): " <> (T.pack $ show up) <> ": " <> (T.pack err))
    Right x -> return x

decodeUnparsed up C.YAML = do
  bs <- C.asIOByteString up
  case YAML.decodeEither' bs of
    Left err -> throwingM _BadParse ("While decoding (as YAML)" <> (T.pack $ show up) <> ": " <> (T.pack $ show err))
    Right x -> return x

decodeUnparsed _ _ = throwingM _BadParse "Fallthrough case in decodeUnparsed."

loadDataSources :: FJ tag =>
  BaseFCC tag -> Maybe FilePath -> [C.DataSource] -> StateT (C.LoadedModels tag) IO ()
loadDataSources fcc mSchemaDir = mapM_ (loadDataSource fcc mSchemaDir)

loadXMLConfigs :: FJ tag
  => BaseFCC tag
  -> Maybe FilePath
  -> C.Unparsed
  -> StateT (C.LoadedModels tag, C.ModelDescriptionMap) IO ()
loadXMLConfigs fcc mSchema (C.UnparsedFile fp) = do
  (sources,configM) <- lift $ XML.loadConfigurations mSchema fp
  zoom _1 $ loadDataSources fcc mSchema sources
  hoist generalize $ _2 %= M.union configM
loadXMLConfigs _ _ _ = lift . E.throwIO $ BadParse "loadXMLConfigs called with Unparsed of sort other than UnparsedFile"

loadJSONConfigs:: FJ tag
  => BaseFCC tag
  -> Maybe FilePath
  -> C.Unparsed
  -> StateT (C.LoadedModels tag, C.ModelDescriptionMap) IO ()
loadJSONConfigs fcc mSchema up = do
  configLBS <- lift $ C.asIOLazyByteString up
  case eitherDecode configLBS of
    Left err -> lift $ throwingM _BadParse (T.pack err)
    Right (C.ConfigurationInputs sources configM) -> do
      zoom _1 $ loadDataSources fcc mSchema sources
      hoist generalize $ _2 %= M.union configM

loadYAMLConfigs :: FJ tag
  => BaseFCC tag
  -> Maybe FilePath
  -> C.Unparsed
  -> StateT (C.LoadedModels tag, C.ModelDescriptionMap) IO ()
loadYAMLConfigs fcc mSchema up = do
  configBS <- lift $ C.asIOByteString up
  case YAML.decodeEither' configBS of
    Left err -> lift $ throwingM _BadParse (T.pack $ show err)
    Right (C.ConfigurationInputs sources configM) -> do
      zoom _1 $ loadDataSources fcc mSchema sources
      hoist generalize $ _2 %= M.union configM

loadConfigurationsS :: FJ tag
  => BaseFCC tag
  -> Maybe FilePath
  -> C.Unparsed
  -> StateT (C.LoadedModels tag, C.ModelDescriptionMap) IO ()
loadConfigurationsS fcc mSchemaP up@(C.UnparsedFile configP) = loadC fcc mSchemaP up where
  loadC = case C.encodingFromSuffix configP of
    C.XML -> loadXMLConfigs
    C.JSON -> loadJSONConfigs
    C.YAML -> loadYAMLConfigs
    C.UnkEncoding -> \_ _ _ -> lift $ throwingM _Other "Bad file type specified as config.  Only .xml .json and .yaml supported"

loadConfigurationsS _ _ _ = lift $ throwingM _BadParse "loadConfigurationS called with Unparsed of sort other than UnparsedFile"


loadConfigurations :: FJ tag
  => BaseFCC tag
  -> Maybe FilePath
  -> C.Unparsed
  -> IO (C.LoadedModels tag, C.ModelDescriptionMap)
loadConfigurations fcc mSchemaP up =
  execStateT (loadConfigurationsS fcc mSchemaP up) (C.LoadedModels M.empty M.empty C.emptyTaxStructure,M.empty)

buildMCState :: (ComponentTypes tag, rm ~ RateModelType tag) => C.InitialFS tag -> FinEnv rm -> MCState tag
buildMCState (C.InitialFS bs cfs les rules sweepR taxTradeR) fe = makeMCState bs cfs fe les rules sweepR taxTradeR

makeStartingRates :: IsRateModel rm => rm -> RateTable Rate
makeStartingRates rateDefaultModel =
  let ((_, startingRates), _) = runModel defaultArrayBasedRateTable rateDefaultModel (pureMT 1) --ICK.  Hard wired pureMT.  Ick.
  in startingRates

buildInitialState :: (ComponentTypes tag, rm ~ RateModelType tag)
  => C.InitialFS tag
  -> TaxRules
  -> RateTable Rate
  -> rm
  -> Year
  -> MV.Currency
  -> (FinEnv rm, CombinedState tag)
buildInitialState ifs taxRules startingRates rModel date ccy =
  let erF = exchangeMatrixFromRateTable startingRates
      fe = FinEnv startingRates erF date ccy taxRules rModel
      mcs = buildMCState ifs fe
      ics = CombinedState (zeroFinState ccy) mcs False
  in (fe, ics)

getInitialStates :: (ComponentTypes tag, rm ~ RateModelType tag)
  => C.LoadedModels tag
  -> FilingStatus
  -> (String,String,String,String,String,Maybe String)
  -> Year
  -> MV.Currency
  -> Either FMCException (FinEnv rm, CombinedState tag)
getInitialStates (C.LoadedModels ifsM rmM ts) fstat (fsName,rdName,rmName,tfName,tsName,mtcName) date ccy = do
  initialFS <-
    noteM (Other ("Couldn't find Financial State named \"" <> (T.pack fsName) <> "\"")) $ C.getFinancialState ifsM fsName
  taxRules <- C.makeTaxRules ts fstat tfName tsName mtcName
  rateDefaultModel <- noteM (Other "Failed in getRateModel (defaults)") $ C.getRateModel rmM rdName
  rateModel <- noteM (Other "Failed in getRateModel (model)") $ C.getRateModel rmM rmName
  let startingRates  = makeStartingRates rateDefaultModel
  return $ buildInitialState initialFS taxRules startingRates rateModel date ccy


buildInitialStateFromConfig :: (ComponentTypes tag, rm ~ RateModelType tag)
  => C.LoadedModels tag
  -> C.ModelDescriptionMap
  -> String
  -> Either FMCException (Maybe String, FinEnv rm, CombinedState tag)
buildInitialStateFromConfig ci cm confName = do
  (C.ModelDescription fs op date ccy (rds,rms) (fstat,fed,st,cty)) <-
    noteM (Other ("Failed to find conf=" <> (T.pack confName))) $ M.lookup confName cm
  (fe,cs) <- getInitialStates ci fstat (fs,rds,rms,fed,st,cty) date ccy
  return (op,fe,cs)


exchangeRates::MV.ExchangeRateFunction
exchangeRates MV.USD MV.EUR = 0.8
exchangeRates MV.EUR MV.USD = 1.0/exchangeRates MV.USD MV.EUR
exchangeRates _ _           = 1.0


validateFinancialState :: ComponentTypes tag => C.InitialFS tag -> String -> Either FMCException ()
validateFinancialState (C.InitialFS bs _ _ rs sw tt) configName = do
  let validAccountNames = getAccountNames bs
      checkName::AccountName->Either FMCException ()
      checkName name = if name `elem` validAccountNames
                       then Right ()
                       else throwing _Other ("(" <> (T.pack configName)
                                             <> ") Account \"" <> (T.pack $ show name)
                                             <> "\" required by a rule but not found.")
      checkRule rule = foldM (\_ name-> checkName name) () (ruleAccounts rule)
      allRules = rs ++ [sw,tt]
  foldM (\_ r->checkRule r) () allRules
