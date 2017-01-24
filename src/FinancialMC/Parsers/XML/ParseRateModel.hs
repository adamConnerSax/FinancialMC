{-# LANGUAGE Arrows, NoMonomorphismRestriction, FlexibleContexts, RankNTypes, KindSignatures #-}
module FinancialMC.Parsers.XML.ParseRateModel 
       (
         loadRateModelsFromFile,
         loadRateModelsFromString,
       ) where
                       
--import FinancialMC.Core.Rates (RateModel(MkRateModel))
--import FinancialMC.Builders.RateModels (RateModelFactor(MkRateModelFactor))
import qualified FinancialMC.Builders.RateModels as RM
import FinancialMC.Parsers.Configuration (RateModels)

import FinancialMC.Parsers.XML.Utilities (buildOpts,XmlParseInfos,readAttrValue,atTag,runFMCX)

import Text.XML.HXT.Core (withRemoveWS,yes,readString,IOSLA,XIOState,XmlTree,(>>>),ArrowXml,ArrowChoice,ArrowXml,
                          returnA,getChildren,returnA,listA,localPart,getElemName,getAttrValue)

import qualified Data.Map as M

import Control.Monad.State.Strict (put,get,modify,MonadState,StateT,MonadTrans,lift,evalStateT)

type RateModel = RM.BaseRateModel RM.BaseRateModelFactor

loadRateModelsFromFile::(RateModel->rm)->Maybe FilePath->FilePath->StateT (RateModels rm) IO ()
loadRateModelsFromFile f mSchemaDir file =
  lift (readFile file) >>= loadRateModelsFromString f mSchemaDir

  
loadRateModelsFromString::(RateModel->rm)->Maybe FilePath->String->StateT (RateModels rm) IO ()
loadRateModelsFromString f mSchemaDir content = do                         
  let opts = buildOpts mSchemaDir [withRemoveWS yes] "RateModels.rng"
  let xml = readString opts content
  loadRateModels' f xml
  
loadRateModels'::forall rm (t::(* -> *) -> * -> *).(MonadTrans t, MonadState (RateModels rm) (t IO))=>
                 (RateModel->rm)->IOSLA (XIOState XmlParseInfos) XmlTree XmlTree -> t IO ()
loadRateModels' f xml = do
  rms<-get
  result <- lift $ runFMCX (xml >>> parseRateModels rms)
  put . (fmap f) $ head result -- returnA always returns a list even if only 1 result

parseFixedSR::ArrowXml a=>a XmlTree RM.BaseRateModelFactor
parseFixedSR = proc l -> do
  rate <- readAttrValue "rate" -< l
  returnA -< RM.Fixed (rate/100)
  
  
parseNormalSR::ArrowXml a=>a XmlTree RM.BaseRateModelFactor
parseNormalSR = proc l -> do
  mean <- readAttrValue "mean" -< l
  vol  <- readAttrValue "vol"  -< l
  returnA -< RM.Normal (mean/100) (vol/100)
 
parseLogNormalSR::ArrowXml a=>a XmlTree RM.BaseRateModelFactor
parseLogNormalSR = proc l -> do
  mean <- readAttrValue "mean" -< l
  vol  <- readAttrValue "vol"  -< l
  returnA -< RM.makeLogNormalFactor (mean/100) (vol/100)

parseSRModel::(ArrowChoice a, ArrowXml a)=>a XmlTree RM.BaseRateModelFactor
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
  factor <- getChildren >>> parseSRModel -< l 
  returnA -< RM.GroupedModel groupType factor

parseSimple::(ArrowChoice a, ArrowXml a)=>a XmlTree RateModel
parseSimple = proc l -> do
  sTag <- readAttrValue "rateType" -< l
  factor <- getChildren >>> parseSRModel -< l 
  returnA -< RM.SingleFactorModel sTag factor

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
  returnA -< (name,RM.ListModel subModels)

parseRateModels::(ArrowChoice a, ArrowXml a)=>(RateModels RateModel)->a XmlTree (RateModels RateModel)
parseRateModels rms = proc l -> do
  models <- listA (atTag "RateModel" >>> parseMultiModel) -< l
  returnA -< foldl (\m (k,v)->M.insert k v m) rms models

test::IO ()
test = do
  rms <- evalStateT (loadRateModelsFromFile id Nothing "RateModels.xml") M.empty
  print rms
