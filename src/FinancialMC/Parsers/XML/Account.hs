{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Account (getBalanceSheet) where

import FinancialMC.Parsers.XML.Utilities (FMCXmlArrow,atTag,readAttrValue,catMaybes)
import FinancialMC.Parsers.XML.Asset
import FinancialMC.Core.Asset (Asset,Account(Account))
import FinancialMC.Core.MCState (BalanceSheet,insertAccount,makeNewBalanceSheet)
--import qualified FinancialMC.Builders.All as Build

import Text.XML.HXT.Core (XmlTree,getChildren,(>>>),getAttrValue,returnA,listA)
import Control.Monad.State.Strict (execState)
import qualified Data.Text as T

getAssets::FMCXmlArrow XmlTree (Maybe Asset)
getAssets = getChildren >>> getAsset    


getAccount::FMCXmlArrow XmlTree Account                 
getAccount = atTag "Account" >>>
  proc l -> do
    name <- getAttrValue "name" -< l
    ccy <- getAttrValue "currency" -< l
    acType <- readAttrValue "type" -< l
    assets <- listA getAssets -< l
    returnA -< Account (T.pack name) acType (read ccy) (catMaybes assets)


getBalanceSheet::FMCXmlArrow XmlTree BalanceSheet                 
getBalanceSheet = atTag "BalanceSheet" >>> 
  proc l -> do 
    accounts <- listA FinancialMC.Parsers.XML.Account.getAccount -< l
    returnA -< execState (mapM_ insertAccount accounts) makeNewBalanceSheet


{-- 
Should we use catMaybe instead?  That would make non-empty account with whatever assets parsed rather than empty account on any failure. 
--}
