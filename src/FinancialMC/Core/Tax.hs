{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FinancialMC.Core.Tax 
       (
         TaxType(..),
         TaxData,defaultTaxData,
         TaxDataApp,TaxDataAppC,
         addTaxableFlow,addDeductibleFlow,
         carryForwardTaxData,
         fullTaxCV,
         TaxBracket(..),
         TaxBrackets,
         FedCapitalGains(..),
         CapGainBand(..),
         MedicareSurtax(..),
         makeTaxBrackets,
         zeroTaxBrackets,
         buildTaxBracketsFromTops,
         FilingStatus(..),
         TaxRules(TaxRules),HasTaxRules(..),
         safeCapGainRateCV,
         updateTaxRules
       ) where


import FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),HasMoneyValue(..),Currency(USD),ExchangeRateFunction)
--import FinancialMC.Core.MoneyValueOps (MDiv(..),AGroup(..),(*|),(|>|))
import qualified FinancialMC.Core.MoneyValueOps as MV
import FinancialMC.Core.Utilities (FMCException(FailedLookup),noteM)
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.CValued ((|+|),(|-|),(|*|),(|/|),(|>|),(|<|),cvMin,cvMax)

import Data.List (foldl',sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Lazy as M
import Control.Lens (makeClassy,makeLenses,(%=),(^.))

import Control.Monad (liftM2)
import Control.Monad.Reader (ReaderT,ask,lift,MonadReader)
import Control.Monad.State.Strict (StateT,put,get,MonadState)
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadThrow(throwM))

import Data.Aeson (ToJSON(..),FromJSON(..),genericToJSON,genericParseJSON)
import Data.Aeson.Types (Options(..),defaultOptions)
import GHC.Generics (Generic)


data TaxType = OrdinaryIncome | NonPayrollIncome | CapitalGain | Dividend | Inheritance deriving (Enum, Eq, Ord, Show)
data TaxDetails = TaxDetails { _tdInflow:: !MoneyValue, _tdDeductions:: !MoneyValue }
makeClassy ''TaxDetails

instance Show TaxDetails where
  show (TaxDetails inflow deductions) = "in=" ++ show inflow ++ "; ded=" ++ show deductions
    
zeroTaxDetails::Currency->TaxDetails  
zeroTaxDetails c = TaxDetails (MoneyValue 0 c) (MoneyValue 0 c)

addTaxDetails::ExchangeRateFunction->TaxDetails->TaxDetails->TaxDetails
addTaxDetails e (TaxDetails inX dedX) (TaxDetails inY dedY) = TaxDetails x y where
  x = MV.inFirst e (+) inX inY
  y = MV.inFirst e (+) dedX dedY

type TaxFlowFunction a = TaxType->MoneyValue->a

taxInflow::TaxType->MoneyValue->TaxDetails 
taxInflow _ mv = TaxDetails mv (MV.zero USD)

taxDeduction::TaxType->MoneyValue->TaxDetails
taxDeduction _ mv = TaxDetails (MV.zero USD) mv


type TaxMap = M.Map TaxType TaxDetails
data TaxData = TaxData { _tdMap:: !TaxMap, _tdCcy:: !Currency } 
makeLenses ''TaxData  

type TaxDataApp m = StateT TaxData (ReaderT ExchangeRateFunction m)

type TaxDataAppC m = (MonadState TaxData m, MonadReader ExchangeRateFunction m) 

instance Show TaxData where
  show (TaxData tm _) = "Tax Info:" ++ foldl' (\c (k,v) -> c++("\n\t"++show k ++ ": " ++ show v)) [] (M.assocs tm) 

defaultTaxList::Currency->[(TaxType,TaxDetails)]
defaultTaxList c = fmap (\k->(k,zeroTaxDetails c)) [OrdinaryIncome,NonPayrollIncome,CapitalGain,Dividend,Inheritance]

defaultTaxMap::Currency->M.Map TaxType TaxDetails
defaultTaxMap c = M.fromList (defaultTaxList c)

defaultTaxData::Currency->TaxData
defaultTaxData c = TaxData (defaultTaxMap c) c

throwableLookup::(MonadThrow m,Show k,Ord k)=>String->M.Map k v->k->m v
throwableLookup mapName m key = do
  let res = M.lookup key m
  case res of 
    Nothing -> throwM $ FailedLookup ("lookup of " ++ show key ++ " in " ++ mapName ++ " failed!") 
    Just x -> return x


--addTaxFlow::Monad m=>TaxFlowFunction TaxDetails->TaxFlowFunction (TaxDataApp m ())
addTaxFlow::TaxDataAppC m=>TaxFlowFunction TaxDetails->TaxFlowFunction (m ())
addTaxFlow mkDetails tt cf = do
  e <- ask
  tdMap %= M.insertWith (addTaxDetails e) tt (mkDetails tt cf) 

  
addTaxableFlow::TaxDataAppC m=>TaxFlowFunction (m ())
addTaxableFlow = addTaxFlow taxInflow

addDeductibleFlow::TaxDataAppC m=>TaxFlowFunction (m())  
addDeductibleFlow = addTaxFlow taxDeduction


carryForwardTaxDetails::TaxDetails->TaxDetails
carryForwardTaxDetails (TaxDetails (MoneyValue t ccy) (MoneyValue d _)) =
  if t > d 
  then zeroTaxDetails ccy 
  else TaxDetails (MV.zero ccy) (MoneyValue (d-t) ccy)
                  

carryForwardTaxData::(MonadThrow m,TaxDataAppC m)=>m ()
carryForwardTaxData = do
  (TaxData tm ccy) <- get
  let f = throwableLookup "TaxData" tm
  cgd <- f CapitalGain
  dd <-  f Dividend
  let z = zeroTaxDetails ccy
  put $ TaxData (M.fromList [(OrdinaryIncome,z),(NonPayrollIncome,z),(Inheritance,z),
                             (Dividend,carryForwardTaxDetails dd),
                             (CapitalGain,carryForwardTaxDetails cgd)]) ccy

  

data FilingStatus = Single | MarriedFilingJointly deriving (Show,Read,Enum,Ord,Eq,Bounded,Generic,FromJSON,ToJSON)         
data TaxBracket = Bracket !MoneyValue !MoneyValue !Double | TopBracket !MoneyValue !Double deriving (Show,Generic,FromJSON,ToJSON)

data TaxBrackets = TaxBrackets ![TaxBracket] deriving (Generic,FromJSON,ToJSON) -- we don't expose this constructor 

instance Show TaxBrackets where
  show (TaxBrackets tbs) = show tbs

zeroTaxBrackets::TaxBrackets
zeroTaxBrackets = TaxBrackets []

-- NB this assumes they are all same currency.  Which is reasonable.  But still.
makeTaxBrackets::[TaxBracket]->TaxBrackets
makeTaxBrackets tbs = TaxBrackets $ sortBy (comparing g) tbs where
  g (Bracket b _ _) = b ^. mAmount
  g (TopBracket b _) = b ^. mAmount


buildTaxBracketsFromTops::Currency->[(Double,Double)]->Double->TaxBrackets
buildTaxBracketsFromTops ccy bktTops topRate = makeTaxBrackets $ regBrackets++[topBracket] where 
  f (t,r) (pT,bkts) = (t,bkts++[Bracket (MoneyValue pT ccy) (MoneyValue t ccy) r])
  (bottomTop, regBrackets)=  foldr f (0,[]) bktTops 
  topBracket = TopBracket (MoneyValue bottomTop ccy) topRate

                                             
-- CValued versions
getTaxCV::TaxBracket->CV.CVD->CV.CVD
getTaxCV (Bracket b t r) taxable' =
  let b' = CV.fromMoneyValue b
      t' = CV.fromMoneyValue t
  in r |*| cvMin (t' |-| b') (cvMax CV.cvZero (taxable' |-| b'))
  
getTaxCV (TopBracket b r) taxable' =
  let b' = CV.fromMoneyValue b
  in r |*| cvMax CV.cvZero (taxable' |-| b')

computeIncomeTaxCV::TaxBrackets->CV.CVD->CV.CVD
computeIncomeTaxCV (TaxBrackets bkts) income' = foldl f CV.cvZero bkts where
  f taxSoFar bkt = taxSoFar |+| (getTaxCV bkt income')

-- Hand tuned CValued version since we call these a lot    
getTaxCV'::Currency->ExchangeRateFunction->TaxBracket->CV.CVD->CV.CVD
getTaxCV' ccy e (Bracket b t r) taxable'  =
  let tax' = CV.unwrap ccy e  taxable' 
      MoneyValue b' _ = MV.convert b ccy e
      MoneyValue t' _ = MV.convert t ccy e
  in CV.fromMoneyValue $ MoneyValue (r*min (t' - b') (max 0 (tax' - b'))) ccy

getTaxCV' ccy e (TopBracket b r) taxable' =
  let tax' = CV.unwrap ccy e taxable'
      MoneyValue b' _ = MV.convert b ccy e
  in CV.toCV (r*max 0 (tax' - b')) ccy

computeIncomeTaxCV'::Currency->ExchangeRateFunction->TaxBrackets->CV.CVD->CV.CVD
computeIncomeTaxCV' ccy e (TaxBrackets bkts) income' = foldl f (CV.mvZero ccy) bkts where
  f taxSoFar bkt = taxSoFar |+| (getTaxCV' ccy e bkt income')


getTaxCV''::Currency->ExchangeRateFunction->TaxBracket->Double->Double
getTaxCV'' ccy e (Bracket b t r) taxable'  =
  let MoneyValue b' _ = MV.convert b ccy e
      MoneyValue t' _ = MV.convert t ccy e
  in (r*min (t' - b') (max 0 (taxable' - b'))) 

getTaxCV'' ccy e (TopBracket b r) taxable' =
  let MoneyValue b' _ = MV.convert b ccy e
  in (r*max 0 (taxable' - b'))

computeIncomeTaxCV''::Currency->ExchangeRateFunction->TaxBrackets->CV.CVD->CV.CVD
computeIncomeTaxCV'' ccy e (TaxBrackets bkts) incomeCV =
  let income' = CV.unwrap ccy e incomeCV
      f taxSoFar bkt = taxSoFar + (getTaxCV'' ccy e bkt income')
      tax = foldl f 0 bkts 
  in CV.fromMoneyValue $ MoneyValue tax ccy

marginalRateCV::CV.CVD->TaxBrackets->CV.SVD
marginalRateCV v' (TaxBrackets its) = foldl f (CV.toSVD 0) its where
  f cmr (Bracket b _ rate) = CV.cvIf (v' |>| CV.fromMoneyValue b) (CV.toSVD rate) cmr
  f cmr (TopBracket b rate) = CV.cvIf (v' |>| CV.fromMoneyValue b) (CV.toSVD rate) cmr

trueMarginalRateCV::MoneyValue->TaxBrackets->CV.SVD                                 
trueMarginalRateCV mv@(MoneyValue _ c) tb = 
  let one' = CV.toCV 1.0 c
      base' = CV.fromMoneyValue mv
      t1' = computeIncomeTaxCV tb base'
      t2' = computeIncomeTaxCV tb (base' CV.|+| one')
  in (t2' |-| t1') |/| one'


data CapGainBand = CapGainBand { marginalRate::Double, capGainRate::Double } deriving (Generic, Show,ToJSON,FromJSON)

data FedCapitalGains = FedCapitalGains { topRate::Double, bands::[CapGainBand] } deriving (Generic, Show, ToJSON, FromJSON)
data MedicareSurtax = MedicareSurtax { rate::Double, magiThreshold::MoneyValue } deriving (Generic, Show, ToJSON, FromJSON)

data TaxRules = TaxRules {_trFederal:: !TaxBrackets, 
                          _trPayroll:: !TaxBrackets, 
                          _trEstate:: !TaxBrackets,
                          _trFCG:: !FedCapitalGains,
                          _trMedTax:: !MedicareSurtax,
                          _trState:: !TaxBrackets,
                          _trStateCapGain:: !Double,
                          _trCity:: !TaxBrackets } deriving (Show,Generic)
makeClassy ''TaxRules


instance ToJSON TaxRules where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3 }

instance FromJSON TaxRules where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

safeCapGainRateCV::TaxRules->CV.SVD
safeCapGainRateCV tr = (CV.toSVD (tr ^. trStateCapGain)) |+| maxFedCapGainCV

fedCapitalGainRateCV::CV.SVD -> CV.SVD
fedCapitalGainRateCV margRate'  = CV.cvCase [(margRate' |<| CV.toSVD 0.25, CV.toSVD 0),
                                             (margRate' |<| CV.toSVD 0.39, CV.toSVD 0.15)]
                                  (CV.toSVD 0.2)

fedCapitalGainRateCV'::FedCapitalGains->CV.SVD->CV.SVD
fedCapitalGainRateCV' (FedCapitalGains topRate bands) margRate' =
  let cases = (\(CapGainBand mr cgr)->(margRate' |<| CV.toSVD mr, CV.toSVD cgr)) <$> bands
  in CV.cvCase cases (CV.toSVD topRate)

maxFedCapGainCV::CV.SVD
maxFedCapGainCV = fedCapitalGainRateCV (CV.toSVD 1)

fullTaxCV::TaxRules->TaxData->MV.ER (Either SomeException) (MoneyValue,Double)
fullTaxCV (TaxRules federal payroll estate fcg medstax st sCG city) (TaxData tm ccy) = do
  e <- ask
  let cTax = computeIncomeTaxCV'' ccy e
      net' (TaxDetails inFlow deds) = CV.fromMoneyValue inFlow |-| CV.fromMoneyValue deds
      gross (TaxDetails inFlow _) = inFlow
      details tt = lift $ noteM (FailedLookup ("Failed to find " ++ show tt ++ " in TaxMap!")) (M.lookup tt tm)


  ordinaryD <- details OrdinaryIncome             
  nonPayrollD <- details NonPayrollIncome
  capGainD <- details CapitalGain
  divD <- details Dividend
  inheritanceD <- details Inheritance

  let grossPayrollIncome' = CV.fromMoneyValue $ gross ordinaryD
      payrollTax' = cTax payroll grossPayrollIncome'
      netIncome'  = net' ordinaryD |+| net' nonPayrollD
      stateTax' = cTax st netIncome'
      cityTax' = cTax city netIncome'
      adjIncome' =  netIncome' |-| stateTax' |-| cityTax'
      federalTax' = cTax federal adjIncome'
      fedMarginalRate' = marginalRateCV adjIncome' federal
      cgR = (CV.toSVD sCG) |+| fedCapitalGainRateCV' fcg fedMarginalRate'
      netCapGain' =  net' capGainD |+| net' divD -- NB: right now tax policy on divs and cap gains is close enough   
      capGainTax' = netCapGain' |*| cgR
      netInherited' = net' inheritanceD
      inheritanceTax' = cTax estate netInherited'
      grossNonPayrollD' = CV.fromMoneyValue $ gross nonPayrollD
      sum' = foldl (|+|) (CV.mvZero ccy)
      totalNITax' = sum' [stateTax',cityTax',payrollTax',federalTax',capGainTax']
      total' = sum' [totalNITax',inheritanceTax']
      totalNITaxable' = sum' [grossPayrollIncome',grossNonPayrollD',netCapGain']
      rate' = CV.cvIf (totalNITaxable' |>| (CV.mvZero ccy)) (totalNITax' |/| totalNITaxable') (CV.toSVD 0)
      rateER = CV.asERFReader rate'
      totalER = CV.asERMV ccy total'
  liftM2 (,) totalER rateER



inflateTaxBrackets::Double->TaxBrackets->TaxBrackets    
inflateTaxBrackets rate (TaxBrackets tbs) = makeTaxBrackets tbs' where
  r = 1.0 + rate
  inflateBracket (Bracket bb bt br) = Bracket (MV.multiply bb r) (MV.multiply bt r) br 
  inflateBracket (TopBracket bb br) = TopBracket (MV.multiply bb r) br
  tbs' = map inflateBracket tbs
                                      
updateTaxRules::TaxRules->Double->TaxRules
updateTaxRules  (TaxRules fed payroll estate fcg medstax st sCG city) taxBracketInflationRate = newRules where
  fed' = inflateTaxBrackets taxBracketInflationRate fed
  payroll' = inflateTaxBrackets taxBracketInflationRate payroll
  estate' = inflateTaxBrackets taxBracketInflationRate estate
  st' = inflateTaxBrackets taxBracketInflationRate st
  city' = inflateTaxBrackets taxBracketInflationRate city
  newRules = TaxRules fed' payroll' estate' fcg medstax st' sCG city'
  
