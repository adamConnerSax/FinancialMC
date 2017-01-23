{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FunctionalDependencies, OverloadedStrings #-}
module FinancialMC.Parsers.Configuration
       (
         ParseException(..)
       , ModelDescription(..)
       , ModelConfiguration(..)
       , HasModelConfiguration(..) 
       , SimConfiguration(..)
       , HasSimConfiguration(..)
       , LoadedModels(LoadedModels)
       , HasLoadedModels(..)
       , Encoding(..)
       , encodingFromSuffix
       , Unparsed(..)
       , AsIOString(..)
       , AsIOByteString(..)
       , AsIOLazyByteString(..)
       , SourceContents(..)
       , SourceStructure(..)
       , DataSource(..)
       , ModelDescriptionMap
       , ConfigurationInputs(..)
       , InitialFS(..)
       , IFSMap
       , FMCComponentConverters(..)
       , FMCConvertible(..)
       , convertIFSMap
       , getFinancialState
       , RateModels
       , getRateModel
       , MapByFS
       , makeFSMap
       , TaxStructure(..)
       , HasTaxStructure(..)
       , FederalTaxStructure(..)
       , HasFederalTaxStructure(..)
       , StateTaxStructure(..)
       , HasStateTaxStructure(..)
       , CityTaxStructure(..)
       , HasCityTaxStructure(..)
       , emptyTaxStructure
       , mergeTaxStructures
       , makeTaxRules
       ) where



import           FinancialMC.Core.MoneyValue (Currency,MoneyValue)
import           FinancialMC.Core.Tax (FilingStatus,TaxBrackets,FedCapitalGains,MedicareSurtax(..),zeroTaxBrackets,TaxRules(..))
import           FinancialMC.Core.Rates (RateModel)
import           FinancialMC.Core.MCState (BalanceSheet,CashFlows)
import           FinancialMC.Core.Rule (IsRule)
import           FinancialMC.Core.LifeEvent (IsLifeEvent)
import           FinancialMC.Core.Utilities (Year,noteM,FMCException(..))
import           FinancialMC.Parsers.JSON.Utilities (EnumKeyMap(..))

import           Data.Aeson (ToJSON(..),FromJSON(..),genericToJSON,genericParseJSON,object,(.=),(.:),Value(Object))
import           Data.Aeson.TH (deriveJSON,Options(..),defaultOptions)

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.State.Strict (State,get,put)
import           Data.Aeson.Existential (genericEnvParseJSON,EnvFromJSON(..))

import           Data.List.Split (splitOn)
import qualified Data.Map as M
import           GHC.Generics (Generic)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Word (Word64)
import qualified Control.Exception as E

data ParseException = ParseException String deriving (Show)

instance E.Exception ParseException  

data ModelDescription = ModelDescription { mdFinState::String,
                                           mdOutputPrefix::Maybe String,
                                           mdCurDate::Year,
                                           mdCurrency::Currency,
                                           mdRates::(String,String),
                                           mdTax::(FilingStatus,String,String,Maybe String) } deriving (Show,Generic)

instance ToJSON ModelDescription where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier= drop 2}
instance FromJSON ModelDescription where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier= drop 2}
{- $(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''ModelDescription) -}

data Encoding = XML | JSON | YAML | UnkEncoding deriving (Generic,ToJSON,FromJSON)

encodingFromSuffix::FilePath->Encoding
encodingFromSuffix fp = case last $ splitOn "." fp of
    "xml" -> XML
    "json" -> JSON
    "yaml" -> YAML
    _ -> UnkEncoding

data Unparsed = UnparsedFile FilePath | UnparsedString String | UnparsedByteString B.ByteString | UnparsedLazyByteString LB.ByteString deriving (Show,Generic)

instance ToJSON Unparsed where
  toJSON (UnparsedFile fp) = object ["filePath" .= fp]
  toJSON (UnparsedString s) = object ["string" .= s ]
  toJSON (UnparsedByteString bs) = object ["byteString" .= B.unpack bs ]
  toJSON (UnparsedLazyByteString lbs) = object ["lazyByteString" .= LB.unpack lbs]
  

instance FromJSON Unparsed where
  parseJSON (Object v) = parseF v <|> parseS v <|> parseBS v <|> parseLBS v where
    parseF v' = UnparsedFile <$> v' .: "filePath"
    parseS v' = UnparsedString <$> v' .: "string"
    parseBS v' = UnparsedByteString . BC.pack <$> v' .: "byteString" 
    parseLBS v' = UnparsedLazyByteString . LBC.pack <$> v' .: "lazyByteString"
  parseJSON _ = fail "Non-object in parseJSON::Unparsed"
    
data SourceStructure = Parseable Unparsed Encoding | DBQuery  deriving (Generic,ToJSON,FromJSON)

class AsIOString a where
  asIOString::a->IO String

class AsIOByteString a where
  asIOByteString::a->IO B.ByteString

class AsIOLazyByteString a where
  asIOLazyByteString::a->IO LB.ByteString

instance AsIOString Unparsed where
  asIOString (UnparsedFile fp) = readFile fp
  asIOString (UnparsedString s) = return s
  asIOString (UnparsedByteString bs) = return $ BC.unpack bs
  asIOString (UnparsedLazyByteString lbs) = return $ LBC.unpack lbs

instance AsIOByteString Unparsed where
  asIOByteString (UnparsedFile fp) = B.readFile fp
  asIOByteString (UnparsedString s) = return $ BC.pack s
  asIOByteString (UnparsedByteString bs) = return bs
  asIOByteString (UnparsedLazyByteString lbs) = return $ (B.concat . LB.toChunks) lbs

instance AsIOLazyByteString Unparsed where
  asIOLazyByteString (UnparsedFile fp) = LB.readFile fp
  asIOLazyByteString (UnparsedString s) = return $ LBC.pack s
  asIOLazyByteString (UnparsedByteString bs) = return $ LB.fromStrict bs
  asIOLazyByteString (UnparsedLazyByteString lbs) = return lbs


data SourceContents = FinancialStateS | RateModelS | TaxStructureS deriving (Generic,ToJSON,FromJSON)
data DataSource = DataSource { dsStructure::SourceStructure, dsContents::SourceContents } deriving (Generic)

instance ToJSON DataSource where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier= drop 2}
instance FromJSON DataSource where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier= drop 2} 

{- $(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''DataSource) -}

type ModelDescriptionMap = M.Map String ModelDescription

data ConfigurationInputs = ConfigurationInputs { ciDataSources::[DataSource], ciModelDescriptions::ModelDescriptionMap } deriving (Generic)

instance ToJSON ConfigurationInputs where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier= drop 2}
instance FromJSON ConfigurationInputs where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier= drop 2}

{- $(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''ConfigurationInputs) -}

instance EnvFromJSON e ConfigurationInputs

instance Monoid ConfigurationInputs where
  mempty = ConfigurationInputs [] M.empty
  (ConfigurationInputs ds1 cm1) `mappend` (ConfigurationInputs ds2 cm2) = ConfigurationInputs (ds1 ++ ds2) (M.union cm1 cm2)

data FMCComponentConverters ab a flb fl leb le  rub ru =
  FMCComponentConverters
  {
    assetF::(ab->a),
    flowF::(flb->fl),
    lifeEventF::(leb->le),
    ruleF::(rub->ru)
  }

class FMCConvertible f where
  fmcMap::FMCComponentConverters ab a flb fl leb le rub ru->f ab flb leb rub->f a fl le ru


data InitialFS a fl le ru = InitialFS {ifsBS::BalanceSheet a, 
                                       ifsCF::CashFlows fl, 
                                       ifsLifeEvents::[le],
                                       ifsRules::[ru],
                                       ifsSweep::ru, 
                                       ifsTaxTrade::ru} deriving (Show,Generic)
                           
instance FMCConvertible InitialFS where
  fmcMap (FMCComponentConverters fA fFL fLE fRU) (InitialFS bs cfs les rs sw tax) =
    InitialFS (fA <$> bs) (fFL <$> cfs) (fLE <$> les) (fRU <$> rs) (fRU sw) (fRU tax)

instance (ToJSON a, ToJSON fl, ToJSON le, ToJSON ru)=>ToJSON (InitialFS a fl le ru) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

instance (FromJSON a, FromJSON fl, FromJSON le, FromJSON ru)=>FromJSON (InitialFS a fl le ru) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance (FromJSON a,FromJSON fl,FromJSON le, FromJSON ru)=>EnvFromJSON e (InitialFS a fl le ru)

{-
instance (FromJSON a, FromJSON le, FromJSON fl, FromJSON ru,
          EnvFromJSON e le,
          EnvFromJSON e fl,
          EnvFromJSON e ru,
          EnvFromJSON e (BalanceSheet a),
          EnvFromJSON e (CashFlows fl),
          EnvFromJSON e Rule) => EnvFromJSON e (InitialFS a fl le) where
  envParseJSON = genericEnvParseJSON defaultOptions {fieldLabelModifier = drop 3}
-}
                 
type IFSMap a fl le ru = M.Map String (InitialFS a fl le ru)

convertIFSMap::FMCComponentConverters ab a flb fl leb le rub ru->IFSMap ab flb leb rub->IFSMap a fl le ru
convertIFSMap ccs ifsm = fmcMap ccs <$> ifsm 

getFinancialState::IFSMap a fl le ru->String->Maybe (InitialFS a fl le ru)
getFinancialState ifsm name = M.lookup name ifsm

type RateModels = M.Map String RateModel


getRateModel::RateModels->String->Maybe RateModel
getRateModel rms n = M.lookup n rms

type MapByFS = EnumKeyMap FilingStatus
makeFSMap::M.Map FilingStatus a -> MapByFS a
makeFSMap = EnumKeyMap

{-
instance ToJSON a=>ToJSON (MapByFS a) where
  toJSON (MapByFS m) = toJSONEnumMap m

instance FromJSON a=>FromJSON (MapByFS a) where
  parseJSON v = MapByFS <$> fromJSONEnumMap v
-}

data FederalTaxStructure = FederalTaxStructure {
  _ftsIncome::MapByFS TaxBrackets,
  _ftsPayroll::TaxBrackets,
  _ftsEstate::TaxBrackets,
  _ftsCapGainRateBands::FedCapitalGains,
  _ftsMedicareSurtax::(Double,MapByFS MoneyValue)
  } deriving (Generic,Show)

Lens.makeClassy ''FederalTaxStructure

instance ToJSON FederalTaxStructure where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier= drop 4}
instance FromJSON FederalTaxStructure where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier= drop 4}
  
{- $(deriveJSON defaultOptions{fieldLabelModifier= drop 4} ''FederalTaxStructure) -}

data StateTaxStructure = StateTaxStructure {
  _stsIncome::MapByFS TaxBrackets,
  _stsCapGainRate::Double } deriving (Generic,Show)

Lens.makeClassy ''StateTaxStructure

instance ToJSON StateTaxStructure where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 4}
instance FromJSON StateTaxStructure where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 4}

{- $(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''StateTaxStructure) -}


data CityTaxStructure = CityTaxStructure { _ctsIncome::MapByFS TaxBrackets } deriving (Generic,Show)
Lens.makeClassy ''CityTaxStructure

instance ToJSON CityTaxStructure where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 4}
instance FromJSON CityTaxStructure where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 4}

{-  $(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''CityTaxStructure) -}

data TaxStructure = TaxStructure {
  _tsFederal::M.Map String FederalTaxStructure,
  _tsState::M.Map String StateTaxStructure,
  _tsCity::M.Map String CityTaxStructure } deriving (Show,Generic)

Lens.makeClassy ''TaxStructure

instance ToJSON TaxStructure where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 3}
instance FromJSON TaxStructure where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 3}

{- $(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''TaxStructure) -}

instance EnvFromJSON e TaxStructure

lookupTS::M.Map String a->String->String->Either E.SomeException a
lookupTS taxMap key taxLevelString =
  noteM (FailedLookup ("Couldn't find " ++ show key ++ " in loaded " ++ taxLevelString ++ " tax structures.")) $ M.lookup key taxMap

lookupTB::M.Map FilingStatus TaxBrackets->FilingStatus->String->Either E.SomeException TaxBrackets
lookupTB bracketMap filingStatus taxName =
  noteM (FailedLookup ("Couldn't find " ++ show filingStatus ++ " in loaded " ++ taxName ++ " tax structures.")) $ M.lookup filingStatus bracketMap

makeTaxRules::TaxStructure->FilingStatus->String->String->Maybe String->Either E.SomeException TaxRules
makeTaxRules (TaxStructure fedByName stateByName cityByName) fs fedName stateName mCityName = do
  (FederalTaxStructure fedInc payroll estate cgrb (medSRate,medSThresh)) <- lookupTS fedByName fedName "federal"
  fedT <- lookupTB (unEnumKeyMap fedInc) fs fedName
  msThresh <- noteM (FailedLookup ("Couldn't find " ++ show fs ++ " in Medicare Surtax MAGI thresholds.")) $ M.lookup fs (unEnumKeyMap medSThresh)
  (StateTaxStructure stateInc stateCG) <- lookupTS stateByName stateName "state"
  stateT <- lookupTB (unEnumKeyMap stateInc) fs stateName
  cityT <- case mCityName of
    Nothing -> return zeroTaxBrackets
    Just n -> lookupTS cityByName n "city" >>= (\(CityTaxStructure bktMap)->lookupTB (unEnumKeyMap bktMap) fs n)
  return $ TaxRules fedT payroll estate cgrb (MedicareSurtax medSRate msThresh) stateT stateCG cityT

emptyTaxStructure::TaxStructure
emptyTaxStructure = TaxStructure M.empty M.empty M.empty


mergeTaxStructures::TaxStructure->State TaxStructure ()
mergeTaxStructures (TaxStructure fedN stateN cityN) = do
  TaxStructure fedO stateO cityO <- get
  put $ TaxStructure (M.union fedN fedO) (M.union stateN stateO) (M.union cityN cityO)

  

data LoadedModels a fl le ru = LoadedModels {  _lmFS::IFSMap a fl le ru, _lmRM::RateModels, _lmTax::TaxStructure }

Lens.makeClassy ''LoadedModels


data ModelConfiguration a fl le ru = ModelConfiguration { _mcfgInitialFS::InitialFS a fl le ru,
                                                          _mcfgStartingRM::RateModel,
                                                          _mcfgRateModel::RateModel,
                                                          _mcfgTaxRules::TaxRules,
                                                          _mcfgYear::Year,
                                                          _mcfgCCY::Currency } deriving (Generic)

Lens.makeClassy ''ModelConfiguration


instance FMCConvertible ModelConfiguration where
  fmcMap converters (ModelConfiguration ifs srm rm tr y c) = ModelConfiguration (fmcMap converters ifs) srm rm tr y c
  
instance (ToJSON le, ToJSON fl, ToJSON a, ToJSON ru) => ToJSON (ModelConfiguration a fl le ru) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 5}

instance (FromJSON a, FromJSON le, FromJSON fl, EnvFromJSON e (InitialFS a fl le ru),
          EnvFromJSON e RateModel,EnvFromJSON e TaxRules) => EnvFromJSON e (ModelConfiguration a fl le ru) where
  envParseJSON = genericEnvParseJSON defaultOptions {fieldLabelModifier = drop 5}

data SimConfiguration = SimConfiguration { _scfgYears::Int
                                         , _scfgPaths::Int
                                         , _scfgBins::Int
                                         , _scfgQuantiles::Int
                                         , _scfgSeed::Maybe Word64
                                         } deriving (Generic)

Lens.makeClassy ''SimConfiguration

instance ToJSON SimConfiguration where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 5}
instance FromJSON SimConfiguration where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 5}

{- $(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''SimConfiguration) -}

instance EnvFromJSON e SimConfiguration 
