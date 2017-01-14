{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Rule (getRules,getSweepRule,getTaxTradeRule) where

import           FinancialMC.Parsers.XML.Utilities (readAttrValue,readAttrValueElse,atTag,FMCXmlArrow,addInfoA,XmlParseInfo(Error),catMaybes)

import           FinancialMC.Core.Rule (Rule(MkRule))
import           FinancialMC.Core.TradingTypes (TradeType(Normal))
import           FinancialMC.Core.Utilities (DateRange(..))
import           FinancialMC.Core.Asset (AccountName)

import qualified FinancialMC.Builders.Rules as Rules

import           Text.XML.HXT.Core (returnA,ArrowXml,XmlTree,constA,getChildren,getElemName,localPart,(>>>),getText,listA,orElse,getAttrValue)


import qualified Data.Text as T
    
getSweepRule::ArrowXml a=>a XmlTree Rule    
getSweepRule = atTag "SweepAccount" >>> getChildren >>>
  proc l -> do
    account <- getText -< l
    returnA -< MkRule $ Rules.Sweep (T.pack account)
    
getTaxTradeRule::ArrowXml a=>a XmlTree Rule
getTaxTradeRule = atTag "TaxTradeAccount" >>> getChildren >>>
  proc l -> do
    account <- getText -< l
    returnA -< MkRule $ Rules.TaxTrade (T.pack account)
        
---      
      
getPayFromRule::ArrowXml a=>a XmlTree (Maybe Rule)    
getPayFromRule = proc l -> do
  amountFrom <- getAttrValue "amount_from" -< l
  account <- getAttrValue "account" -< l
  returnA -< Just . MkRule $ Rules.PayFrom (T.pack amountFrom) (T.pack account) (T.pack amountFrom)

getContributionRule::ArrowXml a=>a XmlTree (Maybe Rule)
getContributionRule = proc l -> do
  amount <- readAttrValue "amount" -< l
  account <- getAttrValue "account" -< l
  tradeT <- readAttrValueElse "trade_type" Normal -< l
  dRange <- readAttrValue "when" -< l
  name <- getAttrValue "name" -< l
  returnA -< Just . MkRule $ Rules.Contribution (T.pack name) (T.pack account) amount tradeT dRange
  
getRequiredDistributionRule::ArrowXml a=>a XmlTree (Maybe Rule)
getRequiredDistributionRule = proc l -> do
  account <- getAttrValue "account" -< l
  name <- getAttrValue "name" -< l
  yearTurning70 <- readAttrValue "year_turning_70" -< l 
  returnA -< Just . MkRule $ Rules.RequiredDistribution (T.pack name) (T.pack account) yearTurning70


getPreTaxContributionRule::ArrowXml a=>a XmlTree (Maybe Rule)    
getPreTaxContributionRule = proc l -> do
  amount <- readAttrValue "amount" -< l
  account <- getAttrValue "account" -< l
  dRange <- readAttrValue "when" -< l
  name <- getAttrValue "name" -< l
  returnA -< Just . MkRule $ Rules.makePreTaxContributionRule (T.pack account) amount dRange (T.pack name)

getTransferRule::ArrowXml a=>a XmlTree (Maybe Rule)
getTransferRule = proc l -> do
  fAccount <- getAttrValue "from" -< l
  fTradeType <- readAttrValueElse "from_trade" Normal -< l
  tAccount <- getAttrValue "to" -< l
  tTradeType <- readAttrValueElse "to_trade" Normal -< l
  amount <- readAttrValue "amount" -< l
  dRange <- readAttrValue "when" -< l
  name <- getAttrValue "name" -< l
  returnA -< Just . MkRule $ Rules.Transfer (T.pack name) (T.pack fAccount) fTradeType (T.pack tAccount) tTradeType amount dRange 

getSANAccts::ArrowXml a=>a XmlTree (AccountName,DateRange)    
getSANAccts = atTag "SellFrom" >>>
  proc l -> do
    account <- getAttrValue "account" -< l
    whenAllowed <- readAttrValueElse "allowed" Always -< l
    returnA -< (T.pack account,whenAllowed)

getSellAsNeededRule::ArrowXml a=>a XmlTree (Maybe Rule)    
getSellAsNeededRule = proc l -> do
  accounts <- listA getSANAccts -< l
  returnA -< Just . MkRule $ Rules.SellAsNeeded accounts
  
getCashToInvestmentSweepRule::ArrowXml a=>a XmlTree (Maybe Rule)      
getCashToInvestmentSweepRule = proc l -> do
  cashAcct <- getAttrValue "cash_account" -< l 
  invAcct <- getAttrValue "inv_account" -< l
  minCash <- readAttrValue "min_cash" -< l
  maxCash <- readAttrValue "max_cash" -< l
  returnA -< Just . MkRule $ Rules.CashToInvestmentSweep (T.pack cashAcct) (T.pack invAcct) minCash maxCash


getRules::FMCXmlArrow XmlTree [Rule]    
getRules = (atTag "Rules" >>>
  proc l -> do
    rules <- listA getRule -<l
    returnA -< catMaybes rules) `orElse` constA []


getRule::FMCXmlArrow XmlTree (Maybe Rule)    
getRule = getChildren >>>
  proc l -> do
    tag <- getElemName -< l
    r <- case localPart tag of 
      "PayFrom" -> getPayFromRule -< l
      "Transfer" -> getTransferRule -< l
      "Contribution" -> getContributionRule -< l
      "RequiredDistribution" -> getRequiredDistributionRule -< l
      "PreTaxContribution" -> getPreTaxContributionRule -< l
      "SellAsNeeded" -> getSellAsNeededRule -< l
      "CashToInvestmentSweep" -> getCashToInvestmentSweepRule -< l
      _ -> errorRule (localPart tag) -<< l
    returnA -< r

errorRule::String->FMCXmlArrow XmlTree (Maybe Rule)
errorRule badTag = addInfoA (Error ("Unrecognized Rule Element \"" ++ badTag ++ "\"")) >>> constA Nothing
