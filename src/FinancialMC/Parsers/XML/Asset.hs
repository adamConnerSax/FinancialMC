{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Asset (getAsset) where

import FinancialMC.Parsers.XML.Utilities (readAttrValue,readAttrValueElse,addInfoA,XmlParseInfo(Error),FMCXmlArrow)
import FinancialMC.Core.Asset (Asset(..),AssetCore(..))
import qualified FinancialMC.Core.MoneyValueOps as MV
import qualified FinancialMC.Builders.Assets as Assets 
import qualified Data.Text as T

import Text.XML.HXT.Core (ArrowXml,XmlTree,returnA,(>>>),localPart,getElemName,constA,getAttrValue)

getCash::ArrowXml a=>a XmlTree (Maybe Asset)    
getCash = proc l -> do
  name    <- getAttrValue "name" -< l
  balance <- readAttrValue "balance" -< l
  returnA -< Just . MkAsset $ Assets.CashAsset (AssetCore (T.pack name) balance balance)
                 
getMixedFund::ArrowXml a=>a XmlTree (Maybe Asset)        
getMixedFund = proc l -> do
  name    <- getAttrValue "name" -< l
  pctStock <- readAttrValue "pct_stock" -< l
  stkYld <- readAttrValueElse "stock_div_yield" 0 -< l
  bondInt <- readAttrValueElse "bond_int" 0 -< l
  balance <- readAttrValue "balance" -< l
  basis <- readAttrValue "paid" -< l
  returnA -< Just . MkAsset $ Assets.MixedFund (AssetCore (T.pack name) balance basis) (pctStock/100) (stkYld/100) (bondInt/100)
                 
getGuaranteedFund::ArrowXml a=>a XmlTree (Maybe Asset)        
getGuaranteedFund = proc l -> do
  name    <- getAttrValue "name" -< l
  rate <- readAttrValue "rate" -< l
  balance <- readAttrValue "balance" -< l
  basis <- readAttrValue "paid" -< l
  returnA -< Just . MkAsset $ Assets.GuaranteedFund (AssetCore (T.pack name) balance basis) (rate/100)


getResidentialRealEstate::ArrowXml a=>a XmlTree (Maybe Asset)        
getResidentialRealEstate = proc l -> do    
  name    <- getAttrValue "name" -< l
  value <- readAttrValue "value" -< l
  paid <- readAttrValue "paid" -< l
  returnA -< Just . MkAsset $ Assets.ResidentialRE (AssetCore (T.pack name) value paid)
    
getFixedRateMortgage::ArrowXml a=>a XmlTree (Maybe Asset)        
getFixedRateMortgage = proc l -> do    
  name    <- getAttrValue "name" -< l
  rate <- readAttrValue "rate" -< l
  years <- readAttrValue "years" -< l
  borrowed <- readAttrValue "borrowed" -< l
  remaining <- readAttrValue "remaining" -< l
  returnA -< Just . MkAsset $ Assets.FixedRateMortgage (AssetCore (T.pack name) (MV.negate remaining) (MV.negate borrowed)) (rate/100) years
    
errorAsset::String->FMCXmlArrow XmlTree (Maybe Asset)        
errorAsset badTag = addInfoA (Error ("Unrecognized Asset Element \"" ++ badTag ++ "\"")) >>> constA Nothing 

    
getAsset::FMCXmlArrow XmlTree (Maybe Asset)  
getAsset = proc l -> do
  tag <- getElemName -< l
  let n = localPart tag
  asset <- case n of
    "Cash" -> getCash -< l
    "MixedFund" -> getMixedFund -< l
    "GuaranteedFund" -> getGuaranteedFund -< l
    "ResidentialRealEstate" -> getResidentialRealEstate -< l
    "FixedRateMortgage" -> getFixedRateMortgage -< l
    _ -> errorAsset n -<< l
  returnA -< asset  
