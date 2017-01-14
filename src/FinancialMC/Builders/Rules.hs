{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module FinancialMC.Builders.Rules (
  PayFrom(..),
  Transfer(..),
  Contribution(..),makePreTaxContributionRule,makeAfterTaxContributionRule,  
  RequiredDistribution(..),
  CashToInvestmentSweep(..),
  SellAsNeeded(..),
  Sweep(..),
  TaxTrade(..)
  ) where

import           FinancialMC.Core.Result (ResultT,appendAndReturn)
import           FinancialMC.Core.Utilities (DateRange,Year,mapMSl,between)
import           FinancialMC.Core.MoneyValue (MoneyValue,HasMoneyValue(..),Currency)
import qualified FinancialMC.Core.MoneyValueOps as MV
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.CValued ((|-|),(|/|),(|*|),(|/=|),(|<|),(|<=|),cvIf,cvOr)
import           FinancialMC.Core.Evolve (AccumResult(AddTo,Zero))
import           FinancialMC.Core.Asset (HasAccount(..),accountValueCV,AccountGetter,AccountName,Account)
import           FinancialMC.Core.TradingTypes (TradeType(..),Transaction(..))
import           FinancialMC.Core.FinancialStates (AccumName,FinEnv,HasFinEnv(..),FinState,HasFinState(..),getAccumulatedValue)

import           FinancialMC.Core.Rule (RuleName,IsRule(..),RuleOutput(RuleOutput),RuleWhen(BeforeTax,AfterSweep,Special),RuleApp,showRuleCore)

import           FinancialMC.Core.Tax (safeCapGainRateCV)

import           Control.Lens ((^.),magnify,view)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans.Class (lift)
import           Control.Exception (SomeException)

import           Data.Aeson.Types (Options(fieldLabelModifier),defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Existential (TypeNamed)
import qualified Data.Text as T

import           GHC.Generics (Generic)

mvLift::Monad m=>MV.ER m a->ReaderT FinState (ReaderT FinEnv m) a
mvLift=lift . magnify feExchange

liftFE::ReaderT FinEnv (Either SomeException) a->ReaderT FinState (ReaderT FinEnv (Either SomeException)) a
liftFE = lift

liftFS::ReaderT FinState (ReaderT FinEnv (Either SomeException)) a-> 
        ResultT RuleOutput (ReaderT FinState (ReaderT FinEnv (Either SomeException))) a
liftFS = lift

laERMV::Monad m=>Currency->CV.CVD->ReaderT FinState (ReaderT FinEnv m) MoneyValue
laERMV c = mvLift . CV.asERMV c


liftGetA::AccountGetter->AccountName->ReaderT FinState (ReaderT FinEnv (Either SomeException)) Account
liftGetA getA name = lift . lift $ getA name

--If there is spending (in "accumName"), sell assets from acctName if available to cover with tax treatment from tt
payFrom::PayFrom->AccountGetter->RuleApp ()
payFrom (PayFrom _ acctName accum) getA = do
  output <- lift $ do
    acct <- liftGetA getA acctName
    let ccy = acct ^. acCurrency
        accountBal' = accountValueCV acct
    accumulated <- getAccumulatedValue accum
    toTrade <- laERMV ccy $ CV.cvMax (CV.fromMoneyValue accumulated) (CV.cvNegate accountBal')
    let trade = Transaction acctName Normal toTrade
        accumR = AddTo accum (MV.negate toTrade)
    mvLift . CV.asERFReader $ cvIf (CV.fromMoneyValue toTrade |/=| CV.mvZero ccy) (CV.toSV $ RuleOutput [trade] [accumR]) (CV.toSV $ RuleOutput [] [])
  appendAndReturn output ()


data PayFrom = PayFrom { pfName:: !RuleName, pfAccount:: !AccountName, pfAccumName:: !AccumName } deriving (Generic)
instance Show PayFrom where
  show pf@(PayFrom _ acct accum) = showRuleCore pf ++ " [if possible pays " ++ show accum ++ " from " ++ show acct ++ " (BeforeTax)]"
  
instance TypeNamed PayFrom 
  
instance IsRule PayFrom where  
  ruleName = pfName
  ruleAccounts (PayFrom _ acct _) = [acct] 
  doRule = payFrom
  ruleWhen = const BeforeTax

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''PayFrom)  

transfer::Transfer->AccountGetter->RuleApp ()
transfer (Transfer _ aFrom ttFrom aTo ttTo amt dateRange) _ = do
    curDate <- liftFS . liftFE $ view feCurrentDate
    let tFrom = [Transaction aFrom ttFrom (MV.negate amt) | between curDate dateRange]
        tTo = [Transaction aTo ttTo amt | between curDate dateRange]
    appendAndReturn (RuleOutput (tFrom ++ tTo) []) ()
  
data Transfer = Transfer { trName:: !RuleName, 
                           trFromAcct:: !AccountName, trFromTradeType:: !TradeType,
                           trToAcct:: !AccountName, trToTradeType:: !TradeType,
                           trAmount:: !MoneyValue,
                           trDateRange:: !DateRange} deriving (Generic)
                
instance Show Transfer where
  show t@(Transfer _ fromA fromTT toA toTT amt dr) = showRuleCore t 
                                                     ++ " [transfers " ++ show amt ++ " from " ++ show fromA 
                                                     ++ "(" ++ show fromTT ++ ") to " ++ show toA 
                                                     ++ "(" ++ show toTT ++ "). " 
                                                     ++ show dr ++ " (BeforeTax)]"
instance TypeNamed Transfer
  
instance IsRule Transfer where  
  ruleName = trName
  ruleAccounts (Transfer _ f _ t _ _ _) = [f,t]
  doRule = transfer
  ruleWhen = const BeforeTax
  

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''Transfer)  

contribute::Contribution->AccountGetter->RuleApp ()
contribute (Contribution _ acctName amount tradeType dateRange) _ = do
    curDate <- liftFS . liftFE $ view feCurrentDate
    let trades = [Transaction acctName tradeType amount | between curDate dateRange]
    appendAndReturn (RuleOutput trades []) ()

data Contribution = Contribution { cName:: !RuleName,  
                                   cAcct:: !AccountName,
                                   cAmount:: !MoneyValue,
                                   cTradeType:: !TradeType,
                                   cDateRange:: !DateRange } deriving (Generic)
                    
instance Show Contribution where
  show c@(Contribution _ acct amt tt dr) = showRuleCore c ++ " [contributes " ++ show amt ++ " from " 
                                           ++ show acct ++ " (" ++ show tt ++ "). " ++ show dr ++ " (BeforeTax)]" 
                                           
instance TypeNamed Contribution
                                         
instance IsRule Contribution where 
  ruleName = cName
  ruleAccounts (Contribution _ acct _ _ _) = [acct]
  doRule = contribute
  ruleWhen = const BeforeTax


$(deriveJSON defaultOptions{fieldLabelModifier= drop 1} ''Contribution)  

makePreTaxContributionRule::AccountName->MoneyValue->DateRange->RuleName->Contribution
makePreTaxContributionRule acctName amount dateRange rName = Contribution rName acctName amount Normal dateRange
  
makeAfterTaxContributionRule::AccountName->MoneyValue->DateRange->RuleName->Contribution
makeAfterTaxContributionRule acctName amount dateRange rName = Contribution rName acctName amount OverFund dateRange

lifeExpectancyFrom70::[Double]
lifeExpectancyFrom70 = [17,16.3,15.5,14.8,14.1,13.4,12.7,12.1,11.4,10.8,10.2,
                        9.7,9.1,8.6,8.1,7.6,7.1,6.7,6.3,5.9,5.5,5.2,4.9,4.6,
                        4.3,4.1,3.8,3.6,3.4,3.1,2.9,2.7,2.5,2.3,2.1,1.9,1.7,1.5,1.4,1.2,1.1,1]

toyLifeExpectancyFrom70::[Double]
toyLifeExpectancyFrom70 = [2,1]

distributionFraction::[Double]->Year->Year->Double
distributionFraction expectancies curY year70 = distributionF expectancies (curY-year70)
  
                                                                     
distributionF::[Double]->Int->Double
distributionF exps yearsOver70
  | yearsOver70 < 0           = 0
  | yearsOver70 < length exps = 1.0/(exps !! yearsOver70)
  | otherwise                 = 1
  

requiredDistribution::RequiredDistribution->AccountGetter->RuleApp ()
requiredDistribution (RequiredDistribution _ acctName yearTurning70) getA = do
  trades <- lift $ do
    curDate <- liftFE $ view feCurrentDate
    let fraction = distributionFraction lifeExpectancyFrom70 curDate yearTurning70
    acct <- liftGetA getA acctName 
    amount <- laERMV (acct ^. acCurrency) $ accountValueCV acct |*| (-1.0*fraction)
    return [Transaction acctName Normal amount]
  appendAndReturn (RuleOutput trades []) ()

data RequiredDistribution = RequiredDistribution { rdName:: !RuleName,
                                                   rdAcct:: !AccountName,
                                                   rdYearTurning70:: !Year } deriving (Generic)
                            
  
instance Show RequiredDistribution where  
  show c@(RequiredDistribution _ a y) = showRuleCore c ++ 
                                        " [starting in " ++ show y ++ ", requires distributions from " ++ show a ++ 
                                        " per IRS regulations.]" 
  
instance TypeNamed RequiredDistribution
  
instance IsRule RequiredDistribution where
  ruleName = rdName
  ruleAccounts (RequiredDistribution _ a _) = [a]
  doRule = requiredDistribution 
  ruleWhen = const BeforeTax


$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''RequiredDistribution)  



--rule to keep one account balance between limits by transferring to another.  E.g., keep cash position bounded by buying/selling investments
cashToInvestmentSweep::CashToInvestmentSweep->AccountGetter->RuleApp ()
cashToInvestmentSweep (CashToInvestmentSweep cashAcctName invAcctName minCash maxCash) getA = do
  (cashTrade',invTrade') <- lift $ do
    cashAcct <- liftGetA getA cashAcctName
    invAcct <- liftGetA getA invAcctName
    let ccy = cashAcct ^. acCurrency
        toBankF minC maxC x y = CV.cvCase [(x CV.|<| minC, CV.cvMin (minC CV.|-| x) y),
                                           (x CV.|>| maxC, maxC CV.|-| x)]
                                (CV.mvZero ccy)
    toBank <- laERMV ccy $ toBankF (CV.fromMoneyValue minCash) (CV.fromMoneyValue maxCash) (accountValueCV cashAcct) (accountValueCV invAcct)    
    let cashTrade = Transaction cashAcctName Normal toBank
        invTrade = Transaction invAcctName Normal (MV.negate toBank)
    return (cashTrade, invTrade)
  appendAndReturn (RuleOutput [cashTrade',invTrade'] []) ()

      
data CashToInvestmentSweep = CashToInvestmentSweep { ciCash:: !AccountName,
                                                     ciInv::  !AccountName,
                                                     ciMin::  !MoneyValue,
                                                     ciMax::  !MoneyValue } deriving (Generic)
                             
instance Show CashToInvestmentSweep where
  show ci@(CashToInvestmentSweep cashA invA minC maxC) = showRuleCore ci
                                                       ++ " [moves cash in " ++ show cashA ++ " to/from investments in " 
                                                       ++ show invA ++ " to keep cash between " ++ show minC ++ " and " 
                                                       ++ show maxC ++ ", if possible (AfterSweep)]"
                                                      
  
instance TypeNamed CashToInvestmentSweep

instance IsRule CashToInvestmentSweep where  
  ruleName (CashToInvestmentSweep cA iA _ _) = T.concat [cA,T.pack "<->",iA]  
  ruleAccounts (CashToInvestmentSweep cA iA _ _) = [cA,iA]
  doRule = cashToInvestmentSweep
  ruleWhen = const AfterSweep

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''CashToInvestmentSweep)  

--rule to sell retirement or educational assets instead of bankrupting
--input is list of accounts with penalty rates    
sellAsNeeded::SellAsNeeded->AccountGetter->RuleApp ()
sellAsNeeded (SellAsNeeded as) getA = do -- Rule "EmergencySell" f (fst $ unzip accts) AfterSweep where
  trds <- lift $ do
    cashPos <- view fsCashFlow
    curDate <- liftFE $ view feCurrentDate
    let ccy = cashPos ^. mCurrency
        h::(AccountName,DateRange)->MoneyValue->MV.ER (Either SomeException) (Transaction,MoneyValue)
        h (name,range) need = do
          acct <- lift $ getA name 
          let bal' = accountValueCV acct
              need' = CV.fromMoneyValue need
              toSell' = CV.cvMin need' bal'
              tt = if between curDate range then Normal else EarlyWithdrawal
          toSell <- CV.asERMV ccy (CV.cvNegate toSell')
          let makeT = Transaction name tt toSell
          remainingNeed <- CV.asERMV ccy $ need' |-| toSell'
          return (makeT, remainingNeed)
    mvLift $ if MV.isPositive cashPos  then return [] else mapMSl h (MV.negate cashPos) as 
  appendAndReturn (RuleOutput trds []) ()


data SellAsNeeded = SellAsNeeded { sanAccts::[(AccountName,DateRange)] }  deriving (Generic)

instance Show SellAsNeeded where
  show (SellAsNeeded as) = "SellAsNeeded (if cash is needed, sells, with penalty if outside range, from " ++ show as ++ ")" 
  
instance TypeNamed SellAsNeeded
    
instance IsRule SellAsNeeded where
  ruleName = const $ T.pack "SellAsNeeded"
  ruleAccounts (SellAsNeeded as) = fst $ unzip as 
  doRule = sellAsNeeded
  ruleWhen = const AfterSweep


$(deriveJSON defaultOptions{fieldLabelModifier= drop 3} ''SellAsNeeded)  

taxTrade::TaxTrade->AccountGetter->RuleApp ()
taxTrade (TaxTrade acctName) _ = do
  trades <- lift $ do
    tr <- liftFE $ view feTaxRules
    cashOnHand <- view fsCashFlow
    tax <- getAccumulatedValue (T.pack "TaxOwed") 
    let safeR' = safeCapGainRateCV tr
        ccy = cashOnHand ^. mCurrency
        tax' = CV.fromMoneyValue tax
        cashOnHand' = CV.fromMoneyValue cashOnHand
        needed' = CV.cvMin tax' (tax' |-| cashOnHand')
        amt' = CV.cvNegate $ needed' |/| (CV.toSVD 1.0 |-| safeR')
    amt <- mvLift $ CV.asERMV ccy amt'
    let trade = Transaction acctName Normal amt
    mvLift . CV.asERFReader $ cvIf (cvOr (tax' |<=| CV.mvZero ccy) (tax' |<| cashOnHand')) (CV.toSV []) (CV.toSV [trade]) 
  appendAndReturn (RuleOutput trades [Zero (T.pack "TaxOwed")]) ()


data TaxTrade = TaxTrade { ttAcct:: !AccountName } deriving (Generic)

instance Show TaxTrade where
  show (TaxTrade acct) = "TaxTrade, gets cash to cover taxes from " ++ show acct ++ " (if possible)."
  
instance TypeNamed TaxTrade 
  
instance IsRule TaxTrade where
  ruleName = const $ T.pack "TaxTrade"
  ruleAccounts (TaxTrade a) = [a]
  doRule = taxTrade
  ruleWhen = const Special
$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''TaxTrade)  

--rule to sweep remaining cashPos into given account.  Last rule executed    
sweep::Sweep->AccountGetter->RuleApp ()
sweep (Sweep acctName) _ = do
    cashPos <- liftFS $ view fsCashFlow
    let trade = Transaction acctName Normal cashPos
    appendAndReturn (RuleOutput [trade] []) ()

data Sweep = Sweep { sAcct:: !AccountName } deriving (Generic)

instance Show Sweep where
  show (Sweep acct) = "Sweep: moves excess/gets necessary cash to/from " ++ show acct
  
instance TypeNamed Sweep 
  
instance IsRule Sweep where
  ruleName = const $ T.pack "Sweep"
  ruleAccounts (Sweep a) = [a]
  doRule = sweep
  ruleWhen = const Special
  
$(deriveJSON defaultOptions{fieldLabelModifier= drop 1} ''Sweep)  

