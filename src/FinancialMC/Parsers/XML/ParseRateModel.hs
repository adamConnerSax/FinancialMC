{-# LANGUAGE Arrows, NoMonomorphismRestriction, FlexibleContexts, RankNTypes, KindSignatures #-}
module FinancialMC.Parsers.XML.ParseRateModel 
       (
         loadRateModelsFromFile,
         loadRateModelsFromString,
       ) where
                       
import FinancialMC.Core.Rates (RateModel(MkRateModel))
import FinancialMC.Builders.RateModels (RateModelFactor(MkRateModelFactor))
import qualified FinancialMC.Builders.RateModels as RM
import FinancialMC.Parsers.Configuration (RateModels)

import FinancialMC.Parsers.XML.Utilities (buildOpts,XmlParseInfos,readAttrValue,atTag,runFMCX)

import Text.XML.HXT.Core (withRemoveWS,yes,readString,IOSLA,XIOState,XmlTree,(>>>),ArrowXml,ArrowChoice,ArrowXml,
                          returnA,getChildren,returnA,listA,localPart,getElemName,getAttrValue)

import qualified Data.Map as M

import Control.Monad.State.Strict (put,get,MonadState,StateT,MonadTrans,lift,evalStateT)


loadRateModelsFromFile::Maybe FilePath->FilePath->StateT RateModels IO ()
loadRateModelsFromFile mSchemaDir file =
  lift (readFile file) >>= loadRateModelsFromString mSchemaDir

  
loadRateModelsFromString::Maybe FilePath->String->StateT RateModels IO ()
loadRateModelsFromString mSchemaDir content = do                         
  let opts = buildOpts mSchemaDir [withRemoveWS yes] "RateModels.rng"
  let xml = readString opts content
  loadRateModels' xml
  
loadRateModels'::forall (t::(* -> *) -> * -> *).(MonadTrans t, MonadState RateModels (t IO))=>
                 IOSLA (XIOState XmlParseInfos) XmlTree XmlTree -> t IO ()
loadRateModels' xml = do
  rms<-get
  result <- lift $ runFMCX (xml >>> parseRateModels rms)
  put $ head result -- returnA always returns a list even if only 1 result

parseFixedSR::ArrowXml a=>a XmlTree RateModelFactor
parseFixedSR = proc l -> do
  rate <- readAttrValue "rate" -< l
  returnA -< MkRateModelFactor $ RM.FixedRateModelFactor (rate/100)
  
  
parseNormalSR::ArrowXml a=>a XmlTree RateModelFactor
parseNormalSR = proc l -> do
  mean <- readAttrValue "mean" -< l
  vol  <- readAttrValue "vol"  -< l
  returnA -< MkRateModelFactor $ RM.NormalRateModelFactor (mean/100) (vol/100)
 
parseLogNormalSR::ArrowXml a=>a XmlTree RateModelFactor
parseLogNormalSR = proc l -> do
  mean <- readAttrValue "mean" -< l
  vol  <- readAttrValue "vol"  -< l
  returnA -< MkRateModelFactor $ RM.LogNormalRateModelFactor (mean/100) (vol/100) Nothing Nothing

parseSRModel::(ArrowChoice a, ArrowXml a)=>a XmlTree RateModelFactor
parseSRModel = proc l -> do
  srModelType <- getElemName -< l
  srModel <- case localPart srModelType of
    "Fixed" -> parseFixedSR -< l
    "Normal" -> parseNormalSR -< l
    "LogNormal" -> parseLogNormalSR -< l
--    _ -> parseConstantSR -< l
  returnA -< srModel

parseGrouped::(ArrowChoice a, ArrowXml a)=>a XmlTree RateModel
parseGrouped = proc l -> do
  groupType <- readAttrValue "type" -< l
  model <- getChildren >>> parseSRModel -< l 
  returnA -< MkRateModel $ RM.GroupedFactorModel groupType model

parseSimple::(ArrowChoice a, ArrowXml a)=>a XmlTree RateModel
parseSimple = proc l -> do
  sTag <- readAttrValue "rateType" -< l
  model <- getChildren >>> parseSRModel -< l 
  returnA -< MkRateModel $ RM.SingleFactorModel sTag model

parseRateModel::(ArrowChoice a, ArrowXml a)=>a XmlTree RateModel
parseRateModel = proc l -> do
  modelType <- getElemName -< l
  model <- case localPart modelType of
    "Simple" -> parseSimple -< l
    "TypeGroup" -> parseGrouped -< l
  returnA -< model

parseMultiModel::(ArrowChoice a, ArrowXml a)=>a XmlTree (String,RateModel)
parseMultiModel = proc l -> do
  name <- getAttrValue "name" -< l
  subModels <- listA (getChildren >>> parseRateModel) -< l
  returnA -< (name,MkRateModel $ RM.ListModel subModels)

parseRateModels::(ArrowChoice a, ArrowXml a)=>RateModels->a XmlTree RateModels
parseRateModels rms = proc l -> do
  models <- listA (atTag "RateModel" >>> parseMultiModel) -< l
  returnA -< foldl (\m (k,v)->M.insert k v m) rms models

test::IO ()
test = do
  rms <- evalStateT (loadRateModelsFromFile Nothing "RateModels.xml") M.empty
  print rms
