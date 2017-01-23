{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Rule (getRules,getSweepRule,getTaxTradeRule) where

import           FinancialMC.Parsers.XML.Utilities (FMCXmlArrow,
                                                    XmlParseInfo (Error),
                                                    addInfoA, atTag, catMaybes,
                                                    readAttrValue,
                                                    readAttrValueElse)

import           FinancialMC.Core.Asset            (AccountName)
import           FinancialMC.Core.Rule             (IsRule)
import           FinancialMC.Core.TradingTypes     (TradeType (Normal))
import           FinancialMC.Core.Utilities        (DateRange (..))

import qualified FinancialMC.Builders.Rules        as R

import           Text.XML.HXT.Core                 (ArrowXml, XmlTree, constA,
                                                    getAttrValue, getChildren,
                                                    getElemName, getText, listA,
                                                    localPart, orElse, returnA,
                                                    (>>>))


import qualified Data.Text                         as T

getSweepRule::ArrowXml a=>a XmlTree R.BaseRule
getSweepRule = atTag "SweepAccount" >>> getChildren >>>
  proc l -> do
    account <- getText -< l
    returnA -< R.makeSweepRule (T.pack account)

getTaxTradeRule::ArrowXml a=>a XmlTree R.BaseRule
getTaxTradeRule = atTag "TaxTradeAccount" >>> getChildren >>>
  proc l -> do
    account <- getText -< l
    returnA -< R.makeTaxTradeRule (T.pack account)

---

getPayFromRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getPayFromRule = proc l -> do
  amountFrom <- getAttrValue "amount_from" -< l
  account <- getAttrValue "account" -< l
  returnA -< Just $ R.BaseRule (T.pack amountFrom) (R.PayFrom  (T.pack account) (T.pack amountFrom))

getContributionRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getContributionRule = proc l -> do
  amount <- readAttrValue "amount" -< l
  account <- getAttrValue "account" -< l
  tradeT <- readAttrValueElse "trade_type" Normal -< l
  dRange <- readAttrValue "when" -< l
  name <- getAttrValue "name" -< l
  returnA -< Just $ R.BaseRule (T.pack name) (R.Contribution  (T.pack account) amount tradeT dRange)

getRequiredDistributionRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getRequiredDistributionRule = proc l -> do
  account <- getAttrValue "account" -< l
  name <- getAttrValue "name" -< l
  yearTurning70 <- readAttrValue "year_turning_70" -< l
  returnA -< Just $ R.BaseRule (T.pack name) (R.RequiredDistribution (T.pack account) yearTurning70)


getPreTaxContributionRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getPreTaxContributionRule = proc l -> do
  amount <- readAttrValue "amount" -< l
  account <- getAttrValue "account" -< l
  dRange <- readAttrValue "when" -< l
  name <- getAttrValue "name" -< l
  returnA -< Just $ R.makePreTaxContributionRule (T.pack account) amount dRange (T.pack name)

getTransferRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getTransferRule = proc l -> do
  fAccount <- getAttrValue "from" -< l
  fTradeType <- readAttrValueElse "from_trade" Normal -< l
  tAccount <- getAttrValue "to" -< l
  tTradeType <- readAttrValueElse "to_trade" Normal -< l
  amount <- readAttrValue "amount" -< l
  dRange <- readAttrValue "when" -< l
  name <- getAttrValue "name" -< l
  returnA -< Just $ R.BaseRule (T.pack name) (R.Transfer  (T.pack fAccount) fTradeType (T.pack tAccount) tTradeType amount dRange)

getSANAccts::ArrowXml a=>a XmlTree (AccountName,DateRange)
getSANAccts = atTag "SellFrom" >>>
  proc l -> do
    account <- getAttrValue "account" -< l
    whenAllowed <- readAttrValueElse "allowed" Always -< l
    returnA -< (T.pack account,whenAllowed)

getSellAsNeededRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getSellAsNeededRule = proc l -> do
  accounts <- listA getSANAccts -< l
  returnA -< Just $ R.makeSellAsNeededRule accounts

getCashToInvestmentSweepRule::ArrowXml a=>a XmlTree (Maybe R.BaseRule)
getCashToInvestmentSweepRule = proc l -> do
  cashAcct <- getAttrValue "cash_account" -< l
  invAcct <- getAttrValue "inv_account" -< l
  minCash <- readAttrValue "min_cash" -< l
  maxCash <- readAttrValue "max_cash" -< l
  returnA -< Just $ R.makeCashToInvestmentSweepRule (T.pack cashAcct) (T.pack invAcct) minCash maxCash


getRules::FMCXmlArrow XmlTree [R.BaseRule]
getRules = (atTag "Rules" >>>
  proc l -> do
    rules <- listA getRule -<l
    returnA -< catMaybes rules) `orElse` constA []


getRule::FMCXmlArrow XmlTree (Maybe R.BaseRule)
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

errorRule::String->FMCXmlArrow XmlTree (Maybe R.BaseRule)
errorRule badTag = addInfoA (Error ("Unrecognized Rule Element \"" ++ badTag ++ "\"")) >>> constA Nothing
