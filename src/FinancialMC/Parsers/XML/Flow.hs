{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Flow (getCashFlows) where

import           FinancialMC.Parsers.XML.Utilities (FMCXmlArrow,
                                                    XmlParseInfo (Error),
                                                    addInfoA, atTag, catMaybes,
                                                    readAttrValue,
                                                    readAttrValueDef,
                                                    readAttrValueElse)

import           FinancialMC.Builders.Flows        (BaseFlow (..),
                                                    BaseFlowDetails (..))
import           FinancialMC.Core.Flow             (FlowCore (..), IsFlow)
import           FinancialMC.Core.MCState          (CashFlows, addFlow,
                                                    makeNewCashFlows)
import           FinancialMC.Core.MoneyValue       (Currency (USD),
                                                    MoneyValue (MoneyValue))
import           FinancialMC.Core.Utilities        (DateRange (Always), Frequency (Annually, Monthly))


import           Data.Aeson                        (ToJSON)

import           Control.Monad.State.Strict        (execState)
import qualified Data.Text                         as T
import           Text.XML.HXT.Core                 (XmlTree, constA,
                                                    getAttrValue, getChildren,
                                                    getElemName, listA,
                                                    localPart, returnA, (>>>))

z::MoneyValue
z = MoneyValue 0 USD

getExpense::(FlowCore->BaseFlow)->FMCXmlArrow XmlTree (Maybe BaseFlow)
getExpense expenseMaker = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  freq <- readAttrValueDef "frequency" Monthly -< l
  dRange <- readAttrValueDef "when"  Always -< l
  returnA -< Just $ expenseMaker (FlowCore (T.pack name) amt freq dRange)


getCollegeBill::FMCXmlArrow XmlTree (Maybe BaseFlow)
getCollegeBill = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  amount_to <- getAttrValue "amount_to" -< l
  dRange <- readAttrValueDef "when" Always -< l
  returnA -< Just $ BaseFlow (FlowCore (T.pack name) amt Annually dRange) (EducationExpense (T.pack amount_to))

getHealthcare::FMCXmlArrow XmlTree (Maybe BaseFlow)
getHealthcare = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  ded <- readAttrValueElse "deductible" False -< l
  dRange <- readAttrValueDef "when" Always -< l
  freq <- readAttrValueDef "frequency" Monthly -< l
  returnA -< Just $ BaseFlow (FlowCore (T.pack name) amt freq dRange) (HealthCareExpense ded)

getSalary::FMCXmlArrow XmlTree (Maybe BaseFlow)
getSalary = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  dRange <- readAttrValueDef "when" Always -< l
  returnA -< Just $ BaseFlow (FlowCore (T.pack name) amt Monthly dRange) SalaryPayment

getPayment::FMCXmlArrow XmlTree (Maybe BaseFlow)
getPayment = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  dRange <- readAttrValueDef "when" Always -< l
  freq <- readAttrValueElse "frequency" Annually -< l
  rate <- readAttrValue "growth_rate" -< l
  returnA -< Just $ BaseFlow (FlowCore (T.pack name) amt freq dRange) (Payment (rate/100.0))

getRentalIncome::FMCXmlArrow XmlTree (Maybe BaseFlow)
getRentalIncome = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  freq <- readAttrValueElse "frequency" Monthly -< l
  maxDed <- readAttrValueDef "max_annual_deduction" z -< l
  dRange <- readAttrValueDef "when" Always -< l
  returnA -< Just $ BaseFlow (FlowCore (T.pack name) amt freq dRange) (RentalIncome maxDed)

errorFlow::String->FMCXmlArrow XmlTree (Maybe BaseFlow)
errorFlow badTag = addInfoA (Error ("unrecognized Flow element \"" ++ badTag ++ "\"")) >>> constA Nothing

getFlows::FMCXmlArrow XmlTree (Maybe BaseFlow)
getFlows = getChildren >>> getFlow

getFlow::FMCXmlArrow XmlTree (Maybe BaseFlow)
getFlow = proc l -> do
    tag <- getElemName -< l
    flow <- case localPart tag of
      "Expense" -> getExpense (`BaseFlow` Expense) -< l
      "HealthCareExpense" -> getHealthcare -< l
      "DeductibleExpense" -> getExpense (`BaseFlow` DeductibleExpense) -< l
      "CollegeBill" -> getCollegeBill -< l
      "Salary" -> getSalary -< l
      "Payment" -> getPayment -< l
      "RentalIncome" -> getRentalIncome -< l
      _ -> errorFlow (localPart tag) -<< l
    returnA -< flow

getCashFlows::FMCXmlArrow XmlTree (CashFlows BaseFlow)
getCashFlows = atTag "Flows" >>>
  proc l -> do
    flows <- listA getFlows -< l
    returnA -< execState (mapM_ addFlow (catMaybes flows)) makeNewCashFlows



