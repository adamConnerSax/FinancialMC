{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.LifeEvent (getLifeEvents) where

import FinancialMC.Parsers.XML.Utilities (readAttrValue,readAttrValueElse,FMCXmlArrow,addInfoA,XmlParseInfo(Error),atTag,catMaybes)
import FinancialMC.Core.MoneyValue (Currency(USD))
import FinancialMC.Core.MoneyValueOps (zero)
import FinancialMC.Core.LifeEvent (LifeEvent(MkLifeEvent))
import FinancialMC.Builders.LifeEvents (BuyProperty(BuyProperty))
import Text.XML.HXT.Core (ArrowXml,XmlTree,returnA,(>>>),listA,orElse,getElemName,getChildren,localPart,constA,getAttrValue)
import qualified Data.Text as T
  
getBuyPropertyLE::ArrowXml a=>a XmlTree (Maybe LifeEvent) 
getBuyPropertyLE = proc l -> do
  year <- readAttrValue "year" -< l 
  accountName <- getAttrValue "account_name" -< l
  propertyName <- getAttrValue "property_name" -< l
  propertyValue <- readAttrValue "property_value" -< l
  downPayment <- readAttrValue "down_payment" -< l
  costsInCash <- readAttrValue "costs_in_cash" -< l
  costsInMortgage <- readAttrValue "costs_in_mortgage" -< l
  mortgageRate <- readAttrValue "mortgage_rate" -< l
  mortgageTerm <- readAttrValueElse "mortgage_term" 30 -< l
  annual_insurance <- readAttrValueElse "property_insurance" (zero USD) -< l
  annual_tax <- readAttrValueElse "property_tax" (zero USD) -< l
  annual_maintenance <- readAttrValueElse "annual_maintenance" (zero USD) -< l
  returnA -< Just $ MkLifeEvent $ 
    BuyProperty year (T.pack accountName) (T.pack propertyName) propertyValue 
    downPayment costsInCash costsInMortgage (mortgageRate/100.0) mortgageTerm
    annual_insurance annual_tax annual_maintenance

getLifeEvents::FMCXmlArrow XmlTree [LifeEvent]    
getLifeEvents = (atTag "LifeEvents" >>>
  proc l -> do
    lifeEvents <- listA getLifeEvent -<l
    returnA -< catMaybes lifeEvents) `orElse` constA []


getLifeEvent::FMCXmlArrow XmlTree (Maybe LifeEvent)    
getLifeEvent = getChildren >>>
  proc l -> do
    tag <- getElemName -< l
    le <- case localPart tag of 
      "BuyProperty" -> getBuyPropertyLE -< l
      _ -> errorLifeEvent (localPart tag) -<< l
    returnA -< le

errorLifeEvent::String->FMCXmlArrow XmlTree (Maybe LifeEvent)
errorLifeEvent badTag = addInfoA (Error ("Unrecognized LifeEvent Element \"" ++ badTag ++ "\"")) >>> constA Nothing
