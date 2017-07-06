{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module FinancialMC.Builders.Rules
  (
    BaseRule(..)
  , HasBaseRule (..)
  , BaseRuleDetails(PayFrom,Transfer,RequiredDistribution,Contribution)
  , makePreTaxContributionRule
  , makeAfterTaxContributionRule
  , makeCashToInvestmentSweepRule
  , makeSellAsNeededRule
  , makeTaxTradeRule
  , makeSweepRule
  ) where

import           FinancialMC.Core.Asset           (Account, AccountGetter,
                                                   AccountName, HasAccount (..),
                                                   IsAsset, accountValueCV)
import           FinancialMC.Core.CValued         (cvIf, cvOr, (|*|), (|-|),
                                                   (|/=|), (|/|), (|<=|), (|<|))
import qualified FinancialMC.Core.CValued         as CV
import           FinancialMC.Core.Evolve          (AccumResult (AddTo, Zero))
import           FinancialMC.Core.FinancialStates (AccumName, FinEnv, FinState,
                                                   HasFinEnv (..),
                                                   HasFinState (..),
                                                   getAccumulatedValue)
import           FinancialMC.Core.MoneyValue      (Currency, HasMoneyValue (..),
                                                   MoneyValue)
import qualified FinancialMC.Core.MoneyValueOps   as MV
import           FinancialMC.Core.Result          (ResultT, appendAndReturn)
import           FinancialMC.Core.TradingTypes    (TradeType (..),
                                                   Transaction (..))
import           FinancialMC.Core.Utilities       (DateRange, Year, between,
                                                   mapMSl)

import           FinancialMC.Core.Rule            (IsRule (..), RuleApp,
                                                   RuleName,
                                                   RuleOutput (RuleOutput),
                                                   RuleWhen (AfterSweep, BeforeTax, Special),
                                                   showRuleCore)

import           FinancialMC.Core.Tax             (safeCapGainRateCV)

import           Control.Exception                (SomeException)
import           Control.Lens                     (magnify, makeClassy, view,
                                                   (^.))
import           Control.Monad.Reader             (ReaderT)
import           Control.Monad.Trans.Class        (lift)

import           Data.Aeson                       (FromJSON, ToJSON,
                                                   genericParseJSON,
                                                   genericToJSON)
import           Data.Aeson.Types                 (Options (fieldLabelModifier),
                                                   defaultOptions)
--import           Data.Aeson.TH (deriveJSON)
--import           Data.Aeson.Existential (TypeNamed)
import qualified Data.Text                        as T

import           GHC.Generics                     (Generic)

mvLift :: Monad m => MV.ER m a -> ReaderT FinState (ReaderT (FinEnv rm) m) a
mvLift = lift . magnify feExchange

liftFE :: ReaderT (FinEnv rm) (Either err) a -> ReaderT FinState (ReaderT (FinEnv rm) (Either err)) a
liftFE = lift

liftFS :: ReaderT FinState (ReaderT (FinEnv rm) (Either err)) a->
          ResultT RuleOutput (ReaderT FinState (ReaderT (FinEnv rm) (Either err))) a
liftFS = lift

laERMV :: Monad m=>Currency->CV.CVD->ReaderT FinState (ReaderT (FinEnv rm) m) MoneyValue
laERMV c = mvLift . CV.asERMV c

liftGetA :: AccountGetter a -> AccountName -> ReaderT FinState (ReaderT (FinEnv rm) (Either FMCException)) (Account a)
liftGetA getA name = lift . lift $ getA name

data BaseRuleDetails =
  PayFrom !AccountName !AccumName | -- pay from account to accumulator
  Transfer !AccountName !TradeType !AccountName !TradeType !MoneyValue !DateRange |
  Contribution !AccountName !MoneyValue !TradeType !DateRange |
  RequiredDistribution !AccountName !Year |
  CashToInvestmentSweep !AccountName !AccountName !MoneyValue !MoneyValue |
  SellAsNeeded [(AccountName,DateRange)] |
  TaxTrade !AccountName |
  Sweep !AccountName deriving (Generic,ToJSON,FromJSON)

instance Show BaseRuleDetails where
  show (PayFrom acct accum) = "If possible, pays " ++ show accum ++ " from " ++ show acct ++ " (BeforeTax)"
  show (Transfer fromA fromTT toA toTT amt dr) = "Transfers " ++ show amt ++ " from " ++ show fromA
                                                 ++ "(" ++ show fromTT ++ ") to " ++ show toA
                                                 ++ "(" ++ show toTT ++ "). "
                                                 ++ show dr ++ " (BeforeTax)"
  show (Contribution acct amt tt dr) = "Ccontributes " ++ show amt ++ " from "
                                       ++ show acct ++ " (" ++ show tt ++ "). " ++ show dr ++ " (BeforeTax)"
  show (RequiredDistribution a y) = "Starting in " ++ show y ++ ", requires distributions from " ++ show a ++
                                    " per IRS regulations."
  show (CashToInvestmentSweep cashA invA minC maxC) = "Moves cash in " ++ show cashA ++ " to/from investments in "
                                                      ++ show invA ++ " to keep cash between " ++ show minC ++ " and "
                                                      ++ show maxC ++ ", if possible (AfterSweep)"
  show (SellAsNeeded as) = "If cash is needed, sells, with penalty if outside range, from " ++ show as
  show (TaxTrade acct) = "Gets cash to cover taxes from " ++ show acct ++ " (if possible)."
  show (Sweep acct) = "Moves excess/gets necessary cash to/from " ++ show acct


data BaseRule = BaseRule { _brName :: !RuleName, _brDetails ::  BaseRuleDetails } deriving (Generic,FromJSON,ToJSON)
makeClassy ''BaseRule

instance Show BaseRule where
  show (BaseRule rn rd) = show rn ++ " [" ++ show rd ++ "]"

instance IsRule BaseRule where
  ruleName (BaseRule rn _) = rn
  ruleAccounts (BaseRule _ rd) = baseRuleAccounts rd
  ruleWhen (BaseRule _ rd) = baseRuleWhen rd
  doRule (BaseRule _ rd) = doBaseRule rd

baseRuleAccounts::BaseRuleDetails->[AccountName]
baseRuleAccounts (PayFrom a _)                          = [a]
baseRuleAccounts (Transfer aFrom _ aTo _ _ _)           = [aFrom, aTo]
baseRuleAccounts (Contribution a _ _ _)                 = [a]
baseRuleAccounts (RequiredDistribution a _)             = [a]
baseRuleAccounts (CashToInvestmentSweep aCash aInv _ _) = [aCash, aInv]
baseRuleAccounts (SellAsNeeded sana)                    = fst <$> sana
baseRuleAccounts (TaxTrade a)                           = [a]
baseRuleAccounts (Sweep a)                              = [a]

baseRuleWhen::BaseRuleDetails->RuleWhen
baseRuleWhen (PayFrom _ _)                   = BeforeTax
baseRuleWhen (Transfer _ _ _ _ _ _)          = BeforeTax
baseRuleWhen (Contribution _ _ _ _)          = BeforeTax
baseRuleWhen (RequiredDistribution _ _)      = BeforeTax
baseRuleWhen (CashToInvestmentSweep _ _ _ _) = AfterSweep
baseRuleWhen (SellAsNeeded _)                = AfterSweep
baseRuleWhen (TaxTrade _)                    = Special
baseRuleWhen (Sweep _)                       = Special

doBaseRule :: IsAsset a=>BaseRuleDetails->AccountGetter a->RuleApp rm ()
--If there is spending (in "accumName"), sell assets from acctName if available to cover with tax treatment from tt
doBaseRule (PayFrom acctName accumName) getA = do
  output <- lift $ do
    acct <- liftGetA getA acctName
    let ccy = acct ^. acCurrency
        accountBal' = accountValueCV acct
    accumulated <- getAccumulatedValue accumName
    toTrade <- laERMV ccy $ CV.cvMax (CV.fromMoneyValue accumulated) (CV.cvNegate accountBal')
    let trade = Transaction acctName NormalTrade toTrade
        accumR = AddTo accumName (MV.negate toTrade)
    mvLift . CV.asERFReader $ cvIf (CV.fromMoneyValue toTrade |/=| CV.mvZero ccy) (CV.toSV $ RuleOutput [trade] [accumR]) (CV.toSV $ RuleOutput [] [])
  appendAndReturn output ()

doBaseRule (Transfer aFrom ttFrom aTo ttTo amt dateRange) _ = do
    curDate <- liftFS . liftFE $ view feCurrentDate
    let tFrom = [Transaction aFrom ttFrom (MV.negate amt) | between curDate dateRange]
        tTo = [Transaction aTo ttTo amt | between curDate dateRange]
    appendAndReturn (RuleOutput (tFrom ++ tTo) []) ()

doBaseRule (Contribution acctName amount tradeType dateRange) _ = do
    curDate <- liftFS . liftFE $ view feCurrentDate
    let trades = [Transaction acctName tradeType amount | between curDate dateRange]
    appendAndReturn (RuleOutput trades []) ()

doBaseRule (RequiredDistribution acctName yearTurning70) getA = do
  trades <- lift $ do
    curDate <- liftFE $ view feCurrentDate
    let fraction = distributionFraction lifeExpectancyFrom70 curDate yearTurning70
    acct <- liftGetA getA acctName
    amount <- laERMV (acct ^. acCurrency) $ accountValueCV acct |*| (-1.0*fraction)
    return [Transaction acctName NormalTrade amount]
  appendAndReturn (RuleOutput trades []) ()

--rule to keep one account balance between limits by transferring to another.  E.g., keep cash position bounded by buying/selling investments
doBaseRule (CashToInvestmentSweep cashAcctName invAcctName minCash maxCash) getA = do
  (cashTrade',invTrade') <- lift $ do
    cashAcct <- liftGetA getA cashAcctName
    invAcct <- liftGetA getA invAcctName
    let ccy = cashAcct ^. acCurrency
        toBankF minC maxC x y = CV.cvCase [(x CV.|<| minC, CV.cvMin (minC CV.|-| x) y),
                                           (x CV.|>| maxC, maxC CV.|-| x)]
                                (CV.mvZero ccy)
    toBank <- laERMV ccy $ toBankF (CV.fromMoneyValue minCash) (CV.fromMoneyValue maxCash) (accountValueCV cashAcct) (accountValueCV invAcct)
    let cashTrade = Transaction cashAcctName NormalTrade toBank
        invTrade = Transaction invAcctName NormalTrade (MV.negate toBank)
    return (cashTrade, invTrade)
  appendAndReturn (RuleOutput [cashTrade',invTrade'] []) ()

--rule to sell retirement or educational assets instead of bankrupting
--input is list of accounts with penalty rates
doBaseRule (SellAsNeeded as) getA = do -- Rule "EmergencySell" f (fst $ unzip accts) AfterSweep where
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
              tt = if between curDate range then NormalTrade else EarlyWithdrawal
          toSell <- CV.asERMV ccy (CV.cvNegate toSell')
          let makeT = Transaction name tt toSell
          remainingNeed <- CV.asERMV ccy $ need' |-| toSell'
          return (makeT, remainingNeed)
    mvLift $ if MV.isPositive cashPos  then return [] else mapMSl h (MV.negate cashPos) as
  appendAndReturn (RuleOutput trds []) ()


doBaseRule (TaxTrade acctName) _ = do
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
    let trade = Transaction acctName NormalTrade amt
    mvLift . CV.asERFReader $ cvIf (cvOr (tax' |<=| CV.mvZero ccy) (tax' |<| cashOnHand')) (CV.toSV []) (CV.toSV [trade])
  appendAndReturn (RuleOutput trades [Zero (T.pack "TaxOwed")]) ()

--rule to sweep remaining cashPos into given account.  Last rule executed
doBaseRule (Sweep acctName) _ = do
    cashPos <- liftFS $ view fsCashFlow
    let trade = Transaction acctName NormalTrade cashPos
    appendAndReturn (RuleOutput [trade] []) ()

makeCashToInvestmentSweepRule::AccountName->AccountName->MoneyValue->MoneyValue->BaseRule
makeCashToInvestmentSweepRule from to min max = BaseRule "CashToInvestmentSweep" (CashToInvestmentSweep from to min max)

makeSellAsNeededRule::[(AccountName,DateRange)]->BaseRule
makeSellAsNeededRule as = BaseRule "SellAsNeeded" (SellAsNeeded as)

makeTaxTradeRule::AccountName->BaseRule
makeTaxTradeRule an = BaseRule "TaxTrade" (TaxTrade an)

makeSweepRule::AccountName->BaseRule
makeSweepRule an = BaseRule "Sweep" (Sweep an)

makePreTaxContributionRule::AccountName->MoneyValue->DateRange->RuleName->BaseRule
makePreTaxContributionRule acctName amount dateRange rName = BaseRule rName (Contribution acctName amount NormalTrade dateRange)

makeAfterTaxContributionRule::AccountName->MoneyValue->DateRange->RuleName->BaseRule
makeAfterTaxContributionRule acctName amount dateRange rName = BaseRule rName (Contribution acctName amount OverFund dateRange)

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


{-
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
  ruleAccounts (RequiredDistribution _ an _) = [an]
  doRule = requiredDistribution
  ruleWhen = const BeforeTax


$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''RequiredDistribution)


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


data TaxTrade = TaxTrade { ttAcct:: !AccountName } deriving (Generic)

instance Show TaxTrade where
  show (TaxTrade acct) = "TaxTrade, gets cash to cover taxes from " ++ show acct ++ " (if possible)."

instance TypeNamed TaxTrade

instance IsRule TaxTrade where
  ruleName = const $ T.pack "TaxTrade"
  ruleAccounts (TaxTrade an) = [an]
  doRule = taxTrade
  ruleWhen = const Special
$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''TaxTrade)

data Sweep = Sweep { sAcct:: !AccountName } deriving (Generic)

instance Show Sweep where
  show (Sweep acct) = "Sweep: moves excess/gets necessary cash to/from " ++ show acct

instance TypeNamed Sweep

instance IsRule Sweep where
  ruleName = const $ T.pack "Sweep"
  ruleAccounts (Sweep an) = [an]
  doRule = sweep
  ruleWhen = const Special

$(deriveJSON defaultOptions{fieldLabelModifier= drop 1} ''Sweep)
-}
