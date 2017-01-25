{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Utilities 
       (
         parseXML
       , buildOpts
       , atTag
       , readAttrValue
       , readAttrValueIf
       , readAttrValueElse
       , getAttrValueIf
       , getAttrValueElse
       , readAttrValueDef
       , catMaybes
       , XmlParseInfo(..)
       , XmlParseInfos(..)
       , addInfoA
       , FMCXmlArrow
       , runFMCX
       , getXmlParseInfos
       , split
       , xmlParseError
       ) where

import Text.XML.HXT.Core (SysConfig,withValidate,no,yes,hasName,changeUserState,(>>>),constA,returnA,getUserState,arr,withRemoveWS,
                          XmlTree,first,ArrowXml,XNode,deep,isElem,hasAttr,orElse,getAttrValue,IOStateArrow,readDocument)

import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runXIOState,initialState)
import Text.XML.HXT.RelaxNG (withRelaxNG)
import Data.Tree.NTree.TypeDefs (NTree)
import Text.Read (readEither)

import Control.Monad (when)
import Control.Monad.Catch (throwM)

import FinancialMC.Core.Utilities (FMCException(..))
import Data.Maybe (catMaybes)
import Safe (readNote)

data XmlParseInfo = Info String | Error String deriving (Show)

newtype XmlParseInfos = XmlParseInfos { infoList::[XmlParseInfo] }

instance Show XmlParseInfos where
  show (XmlParseInfos is) = show is

type FMCXmlArrow a b = IOStateArrow XmlParseInfos a b 


deMaybe::[Maybe a]->[a]  
deMaybe = catMaybes

parseXML::String->FMCXmlArrow b XmlTree
parseXML = readDocument [ withValidate no, withRemoveWS yes] 

atTag::ArrowXml a=>String->a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)
  
readAttrValue::(ArrowXml a, Read b)=>String->a XmlTree b
readAttrValue attrName = proc l -> do            
  x <- getAttrValue attrName -< l
  returnA -< readNote ("Reading attribute: " ++ attrName) x
  
readAttrValueEither::(ArrowXml a, Read b)=>String->a XmlTree (Either String b)
readAttrValueEither  attrName = proc l -> do
  x <- getAttrValue attrName -< l
  returnA -< readEither x

readAttrValueDef::Read b=>String->b->FMCXmlArrow XmlTree b
readAttrValueDef name def = proc l -> do
  e <- readAttrValueEither name -< l 
  y <- eitherADef def ("Error parsing a \"" ++ name ++ "\" attribute. ") -< e
  returnA -< y
  
  
readAttrValueIf::(ArrowXml a, Read b)=>String->a XmlTree (Maybe b)
readAttrValueIf name = (hasAttr name >>> readAttrValue name >>> arr Just) `orElse` constA Nothing

readAttrValueElse::(ArrowXml a, Read b)=>String->b->a XmlTree b
readAttrValueElse name defB = (hasAttr name >>> readAttrValue name) `orElse` constA defB

getAttrValueIf::ArrowXml a=>String->a XmlTree (Maybe String)
getAttrValueIf name = (hasAttr name >>> getAttrValue name >>> arr Just) `orElse` constA Nothing

getAttrValueElse::ArrowXml a=>String->String->a XmlTree String
getAttrValueElse name defS = (hasAttr name >>> getAttrValue name) `orElse` constA defS

buildOpts::Maybe String->[SysConfig]->String->[SysConfig]
buildOpts mSchemaDir def_opts rng_name =
  case mSchemaDir of
    Nothing -> def_opts ++ [withValidate no]
    Just schemaDir -> def_opts ++ [withRelaxNG (schemaDir ++ "/" ++ rng_name)]

-- Error handling

addInfo::XmlParseInfo->XmlParseInfos->XmlParseInfos
addInfo info (XmlParseInfos infos) = XmlParseInfos (info:infos)

addInfoA::XmlParseInfo->FMCXmlArrow b b
addInfoA info = changeUserState (\_ infos -> addInfo info infos)

{-
eitherA::String->FMCXmlArrow (Either SomeException a) (Maybe a)
eitherA msgIn = proc l -> do
  r <- case l of
    Left e -> addInfoA (Error (msgIn++show e)) >>> constA Nothing -<< l
    Right x ->  constA (Just x) -<< l
  returnA -< r
-}
  
eitherADef::a->String->FMCXmlArrow (Either String a) a
eitherADef def msgIn = proc l -> do
  r <- case l of
    Left msg -> addInfoA (Error (msgIn++msg)) 
                >>> constA def  -<< l
    Right x ->  constA x -<< l
  returnA -< r

getXmlParseInfos::FMCXmlArrow a XmlParseInfos
getXmlParseInfos = getUserState

split::FMCXmlArrow a (a,a)
split = arr (\x->(x,x))

isXmlParseError::XmlParseInfo->Bool
isXmlParseError (Info _) = False
isXmlParseError (Error _) = True

xmlParseError::XmlParseInfos->Bool
xmlParseError (XmlParseInfos infos) = any isXmlParseError infos --not (null (filter isXmlParseError infos))

xpiMerge::[XmlParseInfos]->XmlParseInfos
xpiMerge li = XmlParseInfos $ li >>= infoList --concatMap (\(XmlParseInfos is)->is) li

runFMCX::FMCXmlArrow XmlTree b->IO [b] 
runFMCX fa = do
  let efa = fa >>> split >>> first getXmlParseInfos
  result <- runXIOState (initialState (XmlParseInfos [])) efa 
  Control.Monad.when (null result) $ throwM (BadParse "Parse Error: no result.") 
  let (xpis',results) = unzip result  
  let xpis = xpiMerge xpis'
  Control.Monad.when (xmlParseError xpis) $ throwM (BadParse ("Parse Error(s): " ++ show xpis))
  return results
          
