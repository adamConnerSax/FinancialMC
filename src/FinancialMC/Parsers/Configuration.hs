{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverloadedStrings, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FunctionalDependencies, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
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
       , convertComponentsInitialFS
       , convertComponentsIFSMap
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
import           FinancialMC.Core.MCState (BalanceSheet,CashFlows,ComponentTypes (..), ShowableComponents, ToJSONComponents, FromJSONComponents)
--import           FinancialMC.Core.Rule (IsRule)
--import           FinancialMC.Core.LifeEvent (IsLifeEvent)
import           FinancialMC.Core.Utilities (Year,noteM,FMCException(..))
import           FinancialMC.Parsers.JSON.Utilities (EnumKeyMap(..))

import           Data.Aeson (ToJSON(..),FromJSON(..),genericToJSON,genericParseJSON,object,(.=),(.:),Value(Object))
import           Data.Aeson.TH (Options(..),defaultOptions)

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.State.Strict (State,get,put)

import           Data.List.Split (splitOn)
import qualified Data.Map as M
import           GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Monoid ((<>))

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


instance Monoid ConfigurationInputs where
  mempty = ConfigurationInputs [] M.empty
  (ConfigurationInputs ds1 cm1) `mappend` (ConfigurationInputs ds2 cm2) = ConfigurationInputs (ds1 ++ ds2) (M.union cm1 cm2)

data FMCComponentConverters tagB tagA rmB rmA where
  FMCComponentConverters :: (ComponentTypes tagA, ComponentTypes tagB) =>
    {
      assetF :: (AssetType tagB -> AssetType tagA),
      flowF :: (FlowType tagB -> FlowType tagA),
      lifeEventF :: (LifeEventType tagB -> LifeEventType tagA),
      ruleF :: (RuleType tagB -> RuleType tagA),
      rateModelF :: (rmB -> rmA)
    } -> FMCComponentConverters tagB tagA rmB rmA

{-
data FMCComponentConverters ab a flb fl leb le  rub ru rmb rm =
  FMCComponentConverters
  {
    assetF::(ab->a),
    flowF::(flb->fl),
    lifeEventF::(leb->le),
    ruleF::(rub->ru),
    rateModelF::(rmb->rm)
  }
-}
{-
class FMCConvertible f where
  fmcMap::FMCComponentConverters ab a flb fl leb le rub ru rmb rm->f ab flb leb rub rmb->f a fl le ru rm
-}

data InitialFS tag where
  InitialFS :: ComponentTypes tag => { ifsBS :: BalanceSheet (AssetType tag)
                                     , ifsCF :: CashFlows (FlowType tag)
                                     , ifsLifeEvents :: [LifeEventType tag]
                                     , ifsRules :: [RuleType tag]
                                     , ifsSweep :: RuleType tag
                                     , ifsTaxTrade :: RuleType tag
                                     } -> InitialFS tag
               
deriving instance (ShowableComponents tag) => Show (InitialFS tag)



data InitialFS' a fl le ru = InitialFS' (BalanceSheet a) (CashFlows fl) [le] [ru] ru ru deriving (Generic)


instance (ToJSON a, ToJSON fl, ToJSON le, ToJSON ru) => ToJSON (InitialFS' a fl le ru) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

instance (FromJSON a, FromJSON fl, FromJSON le, FromJSON ru) => FromJSON (InitialFS' a fl le ru) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSONComponents tag => ToJSON (InitialFS tag) where
  toJSON (InitialFS bs cfs les rus sw tax) = toJSON (InitialFS' bs cfs les rus sw tax)

initialFSFromInitialFS' :: (ComponentTypes tag
                           ,a ~ AssetType tag
                           ,fl ~ FlowType tag
                           ,le ~ LifeEventType tag
                           ,ru ~ RuleType tag) => InitialFS' a fl le ru -> InitialFS tag
initialFSFromInitialFS' (InitialFS' bs cfs les rus sw tax) = InitialFS bs cfs les rus sw tax

instance FromJSONComponents tag => FromJSON (InitialFS tag) where
  parseJSON o = initialFSFromInitialFS' <$> parseJSON o --parsed' where
--    parsed' :: InitialFS' (AssetType tag) (FlowType tag) (LifeEventType tag) (RuleType tag) = parseJSON o

convertComponentsInitialFS :: (ComponentTypes tagA, ComponentTypes tagB) => FMCComponentConverters tagB tagA rmB rmA -> InitialFS tagB -> InitialFS tagA
convertComponentsInitialFS (FMCComponentConverters fA fFL fLE fRU _) (InitialFS bs cfs les rs sw tax) =
  InitialFS (fA <$> bs) (fFL <$> cfs) (fLE <$> les) (fRU <$> rs) (fRU sw) (fRU tax)



                 
type IFSMap tag = M.Map String (InitialFS tag)

convertComponentsIFSMap :: (ComponentTypes tagA, ComponentTypes tagB) => FMCComponentConverters tagB tagA rmB rmA -> IFSMap tagB -> IFSMap tagA
convertComponentsIFSMap ccs ifsm = convertComponentsInitialFS ccs <$> ifsm 

getFinancialState :: IFSMap tag -> String -> Maybe (InitialFS tag)
getFinancialState ifsm name = M.lookup name ifsm

type RateModels rm = M.Map String rm


getRateModel :: RateModels rm -> String -> Maybe rm
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
  _tsFederal :: M.Map String FederalTaxStructure,
  _tsState :: M.Map String StateTaxStructure,
  _tsCity :: M.Map String CityTaxStructure } deriving (Show,Generic)

Lens.makeClassy ''TaxStructure

instance ToJSON TaxStructure where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 3}
instance FromJSON TaxStructure where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 3}

{- $(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''TaxStructure) -}

lookupTS :: M.Map String a -> String -> String -> Either FMCException a
lookupTS taxMap key taxLevelString =
  noteM (FailedLookup ("Couldn't find "
                       <> (T.pack $ show key)
                       <> " in loaded "
                       <> (T.pack taxLevelString)
                       <> " tax structures.")) $ M.lookup key taxMap

lookupTB :: M.Map FilingStatus TaxBrackets -> FilingStatus -> String -> Either FMCException TaxBrackets
lookupTB bracketMap filingStatus taxName =
  noteM (FailedLookup ("Couldn't find "
                       <> (T.pack $ show filingStatus)
                       <> " in loaded "
                       <> (T.pack taxName)
                       <> " tax structures.")) $ M.lookup filingStatus bracketMap

makeTaxRules :: TaxStructure -> FilingStatus -> String -> String -> Maybe String -> Either FMCException TaxRules
makeTaxRules (TaxStructure fedByName stateByName cityByName) fs fedName stateName mCityName = do
  (FederalTaxStructure fedInc payroll estate cgrb (medSRate,medSThresh)) <- lookupTS fedByName fedName "federal"
  fedT <- lookupTB (unEnumKeyMap fedInc) fs fedName
  msThresh <- noteM (FailedLookup ("Couldn't find " <> (T.pack $ show fs) <> " in Medicare Surtax MAGI thresholds.")) $ M.lookup fs (unEnumKeyMap medSThresh)
  (StateTaxStructure stateInc stateCG) <- lookupTS stateByName stateName "state"
  stateT <- lookupTB (unEnumKeyMap stateInc) fs stateName
  cityT <- case mCityName of
    Nothing -> return zeroTaxBrackets
    Just n -> lookupTS cityByName n "city" >>= (\(CityTaxStructure bktMap)->lookupTB (unEnumKeyMap bktMap) fs n)
  return $ TaxRules fedT payroll estate cgrb (MedicareSurtax medSRate msThresh) stateT stateCG cityT

emptyTaxStructure :: TaxStructure
emptyTaxStructure = TaxStructure M.empty M.empty M.empty


mergeTaxStructures::TaxStructure->State TaxStructure ()
mergeTaxStructures (TaxStructure fedN stateN cityN) = do
  TaxStructure fedO stateO cityO <- get
  put $ TaxStructure (M.union fedN fedO) (M.union stateN stateO) (M.union cityN cityO)

  

data LoadedModels tag rm = LoadedModels {  _lmFS :: IFSMap tag, _lmRM :: RateModels rm, _lmTax :: TaxStructure }

Lens.makeClassy ''LoadedModels


data ModelConfiguration tag rm = ModelConfiguration { _mcfgInitialFS :: InitialFS tag,
                                                      _mcfgStartingRM :: rm,
                                                      _mcfgRateModel :: rm,
                                                      _mcfgTaxRules :: TaxRules,
                                                      _mcfgYear :: Year,
                                                      _mcfgCCY :: Currency } deriving (Generic)

deriving instance (ShowableComponents tag, Show rm) => Show (ModelConfiguration tag rm)

Lens.makeClassy ''ModelConfiguration


convertComponentsModelConfiguration :: FMCComponentConverters tagB tagA rmB rmA
                                    -> ModelConfiguration tagB rmB
                                    -> ModelConfiguration tagA rmA
convertComponentsModelConfiguration ccs@(FMCComponentConverters _ _ _ _ rmF) (ModelConfiguration ifs srm rm tr y c) =
  ModelConfiguration (convertComponentsInitialFS ccs ifs) (rmF srm) (rmF rm) tr y c
                                   
  
instance (ToJSON rm, ToJSONComponents tag) => ToJSON (ModelConfiguration tag rm) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 5}

instance (FromJSONComponents tag, FromJSON rm) => FromJSON (ModelConfiguration tag rm) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 5}

data SimConfiguration = SimConfiguration { _scfgYears::Int
                                         , _scfgPaths::Int
                                         , _scfgBins::Int
                                         , _scfgQuantiles::Int
                                         , _scfgSeed::Maybe Word64
                                         } deriving (Show, Generic)

Lens.makeClassy ''SimConfiguration

instance ToJSON SimConfiguration where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 5}
instance FromJSON SimConfiguration where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 5}

{- $(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''SimConfiguration) -}


