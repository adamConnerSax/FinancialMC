{-# LANGUAGE Arrows #-}
module FinancialMC.Parsers.XML.Flow (getCashFlows) where

import FinancialMC.Parsers.XML.Utilities (readAttrValue,readAttrValueDef,readAttrValueElse,addInfoA,atTag,catMaybes,XmlParseInfo(Error),FMCXmlArrow)

import FinancialMC.Core.Flow (IsFlow,Flow(..),FlowCore(..))
import FinancialMC.Core.MCState (CashFlows,addFlow,makeNewCashFlows)
import FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),Currency(USD))
import FinancialMC.Core.Utilities (DateRange(Always),Frequency(Monthly,Annually))
import qualified FinancialMC.Builders.Flows as Flows


import Data.Aeson (ToJSON)

import Text.XML.HXT.Core ((>>>),returnA,getChildren,getElemName,localPart,XmlTree,listA,constA,getAttrValue)
import Control.Monad.State.Strict (execState)
import qualified Data.Text as T

z::MoneyValue
z = MoneyValue 0 USD

getExpense::(IsFlow f, Show f, ToJSON f)=>(FlowCore->f)->FMCXmlArrow XmlTree (Maybe Flow) 
getExpense expenseMaker = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  freq <- readAttrValueDef "frequency" Monthly -< l
  dRange <- readAttrValueDef "when"  Always -< l
  returnA -< Just . MkFlow $ expenseMaker (FlowCore (T.pack name) amt freq dRange)
  
  
getCollegeBill::FMCXmlArrow XmlTree (Maybe Flow)                   
getCollegeBill = proc l -> do  
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  amount_to <- getAttrValue "amount_to" -< l
  dRange <- readAttrValueDef "when" Always -< l
  returnA -< Just . MkFlow $ Flows.EducationalExpense (FlowCore (T.pack name) amt Annually dRange) (T.pack amount_to)

getHealthcare::FMCXmlArrow XmlTree (Maybe Flow)
getHealthcare = proc l -> do
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  ded <- readAttrValueElse "deductible" False -< l
  dRange <- readAttrValueDef "when" Always -< l
  freq <- readAttrValueDef "frequency" Monthly -< l
  returnA -< Just . MkFlow $ Flows.HealthCareExpense (FlowCore (T.pack name) amt freq dRange) ded
                                                      
getSalary::FMCXmlArrow XmlTree (Maybe Flow)                   
getSalary = proc l -> do   
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  dRange <- readAttrValueDef "when" Always -< l
  returnA -< Just . MkFlow $ Flows.SalaryPayment (FlowCore (T.pack name) amt Monthly dRange) 
  
getPayment::FMCXmlArrow XmlTree (Maybe Flow)                   
getPayment = proc l -> do   
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  dRange <- readAttrValueDef "when" Always -< l
  freq <- readAttrValueElse "frequency" Annually -< l
  rate <- readAttrValue "growth_rate" -< l
  returnA -< Just . MkFlow $ Flows.Payment (FlowCore (T.pack name) amt freq dRange) (rate/100.0)
  
getRentalIncome::FMCXmlArrow XmlTree (Maybe Flow)                   
getRentalIncome = proc l -> do  
  name <- getAttrValue "name" -< l
  amt <- readAttrValueDef "amount" z -< l
  freq <- readAttrValueElse "frequency" Monthly -< l
  maxDed <- readAttrValueDef "max_annual_deduction" z -< l
  dRange <- readAttrValueDef "when" Always -< l
  returnA -< Just . MkFlow $ Flows.RentalIncome (FlowCore (T.pack name) amt freq dRange) maxDed
  
errorFlow::String->FMCXmlArrow XmlTree (Maybe Flow)  
errorFlow badTag = addInfoA (Error ("unrecognized Flow element \"" ++ badTag ++ "\"")) >>> constA Nothing
  
getFlows::FMCXmlArrow XmlTree (Maybe Flow)
getFlows = getChildren >>> getFlow

getFlow::FMCXmlArrow XmlTree (Maybe Flow)                 
getFlow = proc l -> do
    tag <- getElemName -< l
    flow <- case localPart tag of
      "Expense" -> getExpense Flows.Expense -< l
      "HealthCareExpense" -> getHealthcare -< l
      "DeductibleExpense" -> getExpense Flows.DeductibleExpense -< l
      "CollegeBill" -> getCollegeBill -< l
      "Salary" -> getSalary -< l
      "Payment" -> getPayment -< l
      "RentalIncome" -> getRentalIncome -< l
      _ -> errorFlow (localPart tag) -<< l
    returnA -< flow

getCashFlows::FMCXmlArrow XmlTree CashFlows                
getCashFlows = atTag "Flows" >>>
  proc l -> do
    flows <- listA getFlows -< l 
    returnA -< execState (mapM_ addFlow (catMaybes flows)) makeNewCashFlows



