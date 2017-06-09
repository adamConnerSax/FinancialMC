{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Asset (getAsset) where

import qualified Data.Text                         as T
import qualified FinancialMC.Builders.Assets       as A
import           FinancialMC.Core.Asset            (AssetCore (..))
import qualified FinancialMC.Core.MoneyValueOps    as MV
import           FinancialMC.Parsers.XML.Utilities (FMCXmlArrow,
                                                    XmlParseInfo (Error),
                                                    addInfoA, readAttrValue,
                                                    readAttrValueElse)

import           Text.XML.HXT.Core                 (ArrowXml, XmlTree, constA,
                                                    getAttrValue, getElemName,
                                                    localPart, returnA, (>>>))

getCash::ArrowXml a=>a XmlTree (Maybe A.BaseAsset)
getCash = proc l -> do
  name    <- getAttrValue "name" -< l
  balance <- readAttrValue "balance" -< l
  returnA -< Just $ A.BaseAsset (AssetCore (T.pack name) balance balance) A.CashAsset

getMixedFund::ArrowXml a=>a XmlTree (Maybe A.BaseAsset)
getMixedFund = proc l -> do
  name    <- getAttrValue "name" -< l
  pctStock <- readAttrValue "pct_stock" -< l
  stkYld <- readAttrValueElse "stock_div_yield" 0 -< l
  bondInt <- readAttrValueElse "bond_int" 0 -< l
  balance <- readAttrValue "balance" -< l
  basis <- readAttrValue "paid" -< l
  returnA -< Just $ A.BaseAsset (AssetCore (T.pack name) balance basis) (A.MixedFund (A.MixedFundDetails (pctStock/100) (stkYld/100) (bondInt/100)))

getGuaranteedFund::ArrowXml a=>a XmlTree (Maybe A.BaseAsset)
getGuaranteedFund = proc l -> do
  name    <- getAttrValue "name" -< l
  rate <- readAttrValue "rate" -< l
  balance <- readAttrValue "balance" -< l
  basis <- readAttrValue "paid" -< l
  returnA -< Just $ A.BaseAsset (AssetCore (T.pack name) balance basis) (A.GuaranteedFund (A.GuaranteedFundDetails (rate/100)))


getResidentialRealEstate::ArrowXml a=>a XmlTree (Maybe A.BaseAsset)
getResidentialRealEstate = proc l -> do
  name    <- getAttrValue "name" -< l
  value <- readAttrValue "value" -< l
  paid <- readAttrValue "paid" -< l
  returnA -< Just $ A.BaseAsset (AssetCore (T.pack name) value paid) A.ResidentialRE

getFixedRateMortgage::ArrowXml a=>a XmlTree (Maybe A.BaseAsset)
getFixedRateMortgage = proc l -> do
  name    <- getAttrValue "name" -< l
  rate <- readAttrValue "rate" -< l
  years <- readAttrValue "years" -< l
  borrowed <- readAttrValue "borrowed" -< l
  remaining <- readAttrValue "remaining" -< l
  returnA -< Just $ A.BaseAsset (AssetCore (T.pack name) (MV.negate remaining) (MV.negate borrowed)) (A.FixedRateMortgage (A.FixedRateMortgageDetails (rate/100) years))

errorAsset::String->FMCXmlArrow XmlTree (Maybe A.BaseAsset)
errorAsset badTag = addInfoA (Error ("Unrecognized Asset Element \"" ++ badTag ++ "\"")) >>> constA Nothing


getAsset::FMCXmlArrow XmlTree (Maybe A.BaseAsset)
getAsset = proc l -> do
  tag <- getElemName -< l
  let n = localPart tag
  asset <- case n of
    "Cash"                  -> getCash -< l
    "MixedFund"             -> getMixedFund -< l
    "GuaranteedFund"        -> getGuaranteedFund -< l
    "ResidentialRealEstate" -> getResidentialRealEstate -< l
    "FixedRateMortgage"     -> getFixedRateMortgage -< l
    _                       -> errorAsset n -<< l
  returnA -< asset
