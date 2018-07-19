{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module FinancialMC.Core.Tax
       (
         TaxType(..)
       , TaxData
       , HasTaxData (..)
       , ReadsTaxData (..)
       , zeroTaxData
       , TaxDataAppC
       , addTaxableFlow
       , addDeductibleFlow
       , carryForwardTaxData
       , fullTaxEDSL
       , TaxBracket(..)
       , TaxBrackets
       , taxBrackets
       , FedCapitalGains(..)
       , CapGainBand(..)
       , MedicareSurtax(..)
       , makeTaxBrackets
       , zeroTaxBrackets
       , buildTaxBracketsFromTops
       , FilingStatus(..)
       , TaxRules(TaxRules)
       , HasTaxRules(..)
       , safeCapGainRateCV
       , updateTaxRules
       , fullTaxCV
       ) where


import           FinancialMC.Core.CValued       (cvMax, cvMin, (|*|), (|+|),
                                                 (|-|), (|/|), (|<|), (|>|))
import qualified FinancialMC.Core.CValued       as CV
import           FinancialMC.Core.MoneyValue    (Currency (USD),
                                                 ExchangeRateFunction,
                                                 HasMoneyValue (..),
                                                 MoneyValue (MoneyValue),
                                                 ReadsExchangeRateFunction (..))
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Utilities     (AsFMCException (..),
                                                 FMCException (FailedLookup),
                                                 noteM)

import           Control.Lens                   (Getter, makeClassy, makeLenses,
                                                 use, (%=), (.=), (^.))
import qualified Data.Array                     as A
import           Data.List                      (foldl', sortBy)
import qualified Data.Map.Lazy                  as M
import           Data.Ord                       (comparing)

import           Control.Monad                  (liftM2)
import           Control.Monad.Error.Lens       (throwing)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.Reader           (MonadReader, ReaderT, ask,
                                                 lift)
import           Control.Monad.State.Strict     (MonadState, StateT, get, put)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T

import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 genericParseJSON,
                                                 genericToJSON)
import           Data.Aeson.Types               (Options (..), defaultOptions)
import           GHC.Generics                   (Generic)

import           TaxEDSL.Core                   (BracketType, TaxType (..))
import qualified TaxEDSL.Core                   as TE
import           TaxEDSL.Money                  (Money (Money))
import           TaxEDSL.TaxPolicies            (basePolicy)

--data TaxType = OrdinaryIncome | NonPayrollIncome | CapitalGain | Dividend | Inheritance deriving (Enum, Eq, Ord, Bounded, A.Ix, Show)
data TaxDetails = TaxDetails { _tdInflow:: !MoneyValue, _tdDeductions:: !MoneyValue }
makeClassy ''TaxDetails

instance Show TaxDetails where
  show (TaxDetails inflow deductions) = "in=" ++ show inflow ++ "; ded=" ++ show deductions

zeroTaxDetails :: Currency -> TaxDetails
zeroTaxDetails c = TaxDetails (MoneyValue 0 c) (MoneyValue 0 c)

addTaxDetails :: ExchangeRateFunction -> TaxDetails -> TaxDetails -> TaxDetails
addTaxDetails e (TaxDetails inX dedX) (TaxDetails inY dedY) = TaxDetails x y where
  x = MV.inFirst e (+) inX inY
  y = MV.inFirst e (+) dedX dedY

moneyValueToMoney :: ExchangeRateFunction -> Currency -> MoneyValue -> Money Double
moneyValueToMoney erf c mv = let (MoneyValue amt _) = MV.convert mv c erf in Money amt

detailsToFlow :: ExchangeRateFunction -> Currency -> TaxDetails -> TE.TaxFlow Double
detailsToFlow erf c (TaxDetails i d) = let f = moneyValueToMoney erf c in TE.TaxFlow (f i) (f d)

type TaxFlowFunction a = TaxType -> MoneyValue -> a

taxInflow :: TaxType -> MoneyValue -> TaxDetails
taxInflow _ mv = TaxDetails mv (MV.zero USD)

taxDeduction :: TaxType -> MoneyValue -> TaxDetails
taxDeduction _ mv = TaxDetails (MV.zero USD) mv

type TaxArray = A.Array TaxType TaxDetails
data TaxData = TaxData { _tdArray:: !TaxArray, _tdCcy:: !Currency }
makeClassy ''TaxData

mvArrayToMoneyArray :: ExchangeRateFunction -> Currency -> TaxArray -> TE.TaxFlows Double
mvArrayToMoneyArray erf c = fmap (detailsToFlow erf c)

class ReadsTaxData s where
  getTaxData :: Getter s TaxData
  default getTaxData :: HasTaxData s => Getter s TaxData
  getTaxData = taxData

type TaxDataAppC s m = (HasTaxData s, ReadsExchangeRateFunction s, MonadState s m) --MonadState TaxData m, MonadReader ExchangeRateFunction m)

instance Show TaxData where
  show (TaxData ta _) = "Tax Info:" ++ foldl' (\c (k,v) -> c++("\n\t"++show k ++ ": " ++ show v)) [] (A.assocs ta)

zeroTaxArray :: Currency -> TaxArray
zeroTaxArray c = A.listArray (minBound, maxBound) (repeat $ zeroTaxDetails c)

zeroTaxData :: Currency -> TaxData
zeroTaxData c = TaxData (zeroTaxArray c) c

addTaxFlow :: TaxDataAppC s m => TaxFlowFunction TaxDetails -> TaxFlowFunction (m ())
addTaxFlow mkDetails tt cf = do
  e <- use getExchangeRateFunction
  taxData.tdArray %= (\ta -> let old = ta A.! tt in ta A.// [(tt,(addTaxDetails e (mkDetails tt cf) old))]) --M.insertWith (addTaxDetails e) tt (mkDetails tt cf)

addTaxableFlow :: TaxDataAppC s m => TaxFlowFunction (m ())
addTaxableFlow = addTaxFlow taxInflow

addDeductibleFlow :: TaxDataAppC s m => TaxFlowFunction (m())
addDeductibleFlow = addTaxFlow taxDeduction

carryForwardTaxDetails :: TaxDetails -> TaxDetails
carryForwardTaxDetails (TaxDetails (MoneyValue t ccy) (MoneyValue d _)) =
  if t > d
  then zeroTaxDetails ccy
  else TaxDetails (MV.zero ccy) (MoneyValue (d-t) ccy)

carryForwardTaxData :: ({-MonadError FMCException m,-} TaxDataAppC s m) => m ()
carryForwardTaxData = do
  (TaxData ta ccy) <- use taxData
  let cgd = ta A.! CapitalGain
      dd = ta A.! Dividend
      newTaxArray = zeroTaxArray ccy A.// [(Dividend,carryForwardTaxDetails dd),(CapitalGain,carryForwardTaxDetails cgd)]
  taxData .= TaxData newTaxArray ccy
{-
  let z = zeroTaxDetails ccy
  taxData .= TaxData (M.fromList [(OrdinaryIncome,z),(NonPayrollIncome,z),(Inheritance,z),
                                  (Dividend,carryForwardTaxDetails dd),
                                  (CapitalGain,carryForwardTaxDetails cgd)]) ccy
-}


data FilingStatus = Single | MarriedFilingJointly deriving (Show, Read, Enum, Ord, Eq, Bounded, Generic, FromJSON, ToJSON)

data TaxBracket = Bracket !MoneyValue !MoneyValue !Double | TopBracket !MoneyValue !Double deriving (Show, Generic, FromJSON, ToJSON)

taxBracketToTaxBracketM :: ExchangeRateFunction -> Currency -> TaxBracket -> TE.TaxBracketM Double
taxBracketToTaxBracketM erf c (Bracket b t r) = let f = moneyValueToMoney erf c in TE.BracketM (f b) (f t) r
taxBracketToTaxBracketM erf c (TopBracket b r) = TE.TopBracketM (moneyValueToMoney erf c b) r

data TaxBrackets = TaxBrackets ![TaxBracket] deriving (Generic, FromJSON, ToJSON) -- we don't expose this constructor

taxBracketsToTaxBracketsM :: ExchangeRateFunction -> Currency -> TaxBrackets -> TE.TaxBracketsM Double
taxBracketsToTaxBracketsM erf c (TaxBrackets l) = TE.TaxBracketsM $ fmap (taxBracketToTaxBracketM erf c) l

taxBrackets :: TaxBrackets -> [TaxBracket]
taxBrackets (TaxBrackets x) = x

instance Show TaxBrackets where
  show (TaxBrackets tbs) = show tbs

zeroTaxBrackets :: TaxBrackets
zeroTaxBrackets = TaxBrackets []

-- NB this assumes they are all same currency.  Which is reasonable.  But still.
makeTaxBrackets :: [TaxBracket] -> TaxBrackets
makeTaxBrackets tbs = TaxBrackets $ sortBy (comparing g) tbs where
  g (Bracket b _ _)  = b ^. mAmount
  g (TopBracket b _) = b ^. mAmount

buildTaxBracketsFromTops :: Currency -> [(Double,Double)] -> Double -> TaxBrackets
buildTaxBracketsFromTops ccy bktTops tR = makeTaxBrackets $ regBrackets++[topBracket] where
  f (t,r) (pT,bkts) = (t,bkts++[Bracket (MoneyValue pT ccy) (MoneyValue t ccy) r])
  (bottomTop, regBrackets)=  foldr f (0,[]) bktTops
  topBracket = TopBracket (MoneyValue bottomTop ccy) tR


data CapGainBand = CapGainBand { marginalRate :: Double, capGainRate :: Double } deriving (Generic, Show,ToJSON,FromJSON)

capGainBandToCapGainBandM :: CapGainBand -> TE.CapGainBandM Double
capGainBandToCapGainBandM (CapGainBand mr cgr) = TE.CapGainBandM mr cgr

data FedCapitalGains = FedCapitalGains { topRate :: Double, bands :: [CapGainBand] } deriving (Generic, Show, ToJSON, FromJSON)
data MedicareSurtax = MedicareSurtax { payrollRate :: Double, netInvRate :: Double, magiThreshold :: MoneyValue } deriving (Generic, Show, ToJSON, FromJSON)

fedCapitalGainsToFedCapitalGainsM :: FedCapitalGains -> TE.FedCapitalGainsM Double
fedCapitalGainsToFedCapitalGainsM (FedCapitalGains tr bands) = TE.FedCapitalGainsM tr (fmap capGainBandToCapGainBandM bands)

medicareSurtaxToMedicareSurtaxM :: ExchangeRateFunction -> Currency -> MedicareSurtax -> TE.MedicareSurtaxM Double
medicareSurtaxToMedicareSurtaxM erf c (MedicareSurtax pr nir mt) = TE.MedicareSurtaxM pr nir (moneyValueToMoney erf c mt)

data TaxRules = TaxRules {_trFederal      :: !TaxBrackets,
                          _trPayroll      :: !TaxBrackets,
                          _trEstate       :: !TaxBrackets,
                          _trFCG          :: !FedCapitalGains,
                          _trMedTax       :: !MedicareSurtax,
                          _trState        :: !TaxBrackets,
                          _trStateCapGain :: !Double,
                          _trCity         :: !TaxBrackets } deriving (Show, Generic)
makeClassy ''TaxRules

instance ToJSON TaxRules where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3 }

instance FromJSON TaxRules where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

inflateTaxBrackets :: Double -> TaxBrackets -> TaxBrackets
inflateTaxBrackets rt (TaxBrackets tbs) = makeTaxBrackets tbs' where
  r = 1.0 + rt
  inflateBracket (Bracket bb bt br) = Bracket (MV.multiply bb r) (MV.multiply bt r) br
  inflateBracket (TopBracket bb br) = TopBracket (MV.multiply bb r) br
  tbs' = map inflateBracket tbs

updateTaxRules :: TaxRules -> Double -> TaxRules
updateTaxRules  (TaxRules fed payroll estate fcg medstax st sCG city) taxBracketInflationRate = newRules where
  fed' = inflateTaxBrackets taxBracketInflationRate fed
  payroll' = inflateTaxBrackets taxBracketInflationRate payroll
  estate' = inflateTaxBrackets taxBracketInflationRate estate
  st' = inflateTaxBrackets taxBracketInflationRate st
  city' = inflateTaxBrackets taxBracketInflationRate city
  newRules = TaxRules fed' payroll' estate' fcg medstax st' sCG city'


-- The rest is deprecated and has been replaced with TaxEDSL based computation
taxRulesToTaxRulesM :: ExchangeRateFunction -> Currency -> TaxRules -> TE.TaxRulesM Double
taxRulesToTaxRulesM erf c (TaxRules fed payroll estate fcg ms state scg city) =
  let f = taxBracketsToTaxBracketsM erf c
      brackets = A.array (minBound,maxBound)
        [
          (TE.Federal,f fed)
        , (TE.State, f state)
        , (TE.Local, f city)
        , (TE.Payroll, f payroll)
        , (TE.Estate, f estate)
        ]
      fcg' = fedCapitalGainsToFedCapitalGainsM fcg
      ms' = medicareSurtaxToMedicareSurtaxM erf c ms
  in TE.TaxRulesM brackets fcg' ms' scg


fullTaxEDSL :: (MonadError FMCException m
               , MonadState s m
               , ReadsExchangeRateFunction s)
            => TaxRules
            -> TaxData
            -> m (MoneyValue,Double)
fullTaxEDSL tr (TaxData ta ccy) = do
  e <- use getExchangeRateFunction
  let trm = taxRulesToTaxRulesM e ccy tr
      tfs = mvArrayToMoneyArray e ccy ta
      (Money tax, rate) = TE.runTaxMonad (TE.taxReaderProgram $ basePolicy True) (TE.TaxEnv trm tfs)
      taxMV = MoneyValue tax ccy
  return (taxMV, rate)

safeCapGainRateCV :: TaxRules -> CV.SVD
safeCapGainRateCV tr = (CV.toSVD (tr ^. trStateCapGain)) |+| maxFedCapGainCV

fedCapitalGainRateCV :: CV.SVD -> CV.SVD
fedCapitalGainRateCV margRate'  = CV.cvCase [(margRate' |<| CV.toSVD 0.25, CV.toSVD 0),
                                             (margRate' |<| CV.toSVD 0.39, CV.toSVD 0.15)]
                                  (CV.toSVD 0.2)

fedCapitalGainRateCV' :: FedCapitalGains -> CV.SVD -> CV.SVD
fedCapitalGainRateCV' (FedCapitalGains tRate rateBands) margRate' =
  let cases = (\(CapGainBand mr cgr)->(margRate' |<| CV.toSVD mr, CV.toSVD cgr)) <$> rateBands
  in CV.cvCase cases (CV.toSVD tRate)

maxFedCapGainCV :: CV.SVD
maxFedCapGainCV = fedCapitalGainRateCV (CV.toSVD 1)

fullTaxCV :: (MonadError FMCException m, MonadState s m, ReadsExchangeRateFunction s) => TaxRules -> TaxData -> m (MoneyValue,Double)
fullTaxCV (TaxRules federal payroll estate fcg medstax st sCG city) (TaxData ta ccy) = do
  e <- use getExchangeRateFunction
  let cTax = computeIncomeTaxCV'' ccy e
      net' (TaxDetails inFlow deds) = CV.fromMoneyValue inFlow |-| CV.fromMoneyValue deds
      gross (TaxDetails inFlow _) = inFlow
      --details tt = noteM (FailedLookup ("Failed to find " <> (T.pack $ show tt) <> " in TaxMap!")) (M.lookup tt tm)

      ordinaryD = ta A.! OrdinaryIncome
      nonPayrollD = ta A.!  NonPayrollIncome
      capGainD = ta A.! CapitalGain
      divD = ta A.! Dividend
      inheritanceD = ta A.! Inheritance

  let grossPayrollIncome' = CV.fromMoneyValue $ gross ordinaryD
      payrollTax' = cTax payroll grossPayrollIncome'
      netIncome'  = net' ordinaryD |+| net' nonPayrollD
      netCapGain' =  net' capGainD |+| net' divD -- NB: right now tax policy on divs and cap gains is close enough
      stateIncTax' = cTax st netIncome'
      stateCapGainTax' = (CV.toSVD sCG) |*| netCapGain'
      stateTax' = stateIncTax' |+| stateCapGainTax'
      cityTax' = cTax city netIncome'
      adjIncome' =  netIncome' |-| stateTax' |-| cityTax'
      fedIncTax' = cTax federal adjIncome'
      fedMarginalRate' = marginalRateCV adjIncome' federal
      fedCgr = fedCapitalGainRateCV' fcg fedMarginalRate'
      fedCapGainTax' = netCapGain' |*| fedCgr
      federalTax' = fedIncTax' |+| fedCapGainTax'
      netInherited' = net' inheritanceD
      inheritanceTax' = cTax estate netInherited'
      grossNonPayrollD' = CV.fromMoneyValue $ gross nonPayrollD
      sum' = foldl (|+|) (CV.mvZero ccy)
      totalNITax' = sum' [stateTax',cityTax',payrollTax',federalTax']
      total' = sum' [totalNITax',inheritanceTax']
      totalNITaxable' = sum' [grossPayrollIncome',grossNonPayrollD',netCapGain']
      rate' = CV.cvIf (totalNITaxable' |>| (CV.mvZero ccy)) (totalNITax' |/| totalNITaxable') (CV.toSVD 0)
      rateER = CV.asERFReader rate'
      totalER = CV.asERMV ccy total'
  liftM2 (,) totalER rateER
{-# INLINE fullTaxCV #-}


-- CValued versions
getTaxCV :: TaxBracket -> CV.CVD -> CV.CVD
getTaxCV (Bracket b t r) taxable' =
  let b' = CV.fromMoneyValue b
      t' = CV.fromMoneyValue t
  in r |*| cvMin (t' |-| b') (cvMax CV.cvZero (taxable' |-| b'))

getTaxCV (TopBracket b r) taxable' =
  let b' = CV.fromMoneyValue b
  in r |*| cvMax CV.cvZero (taxable' |-| b')

computeIncomeTaxCV :: TaxBrackets -> CV.CVD -> CV.CVD
computeIncomeTaxCV (TaxBrackets bkts) income' = foldl f CV.cvZero bkts where
  f taxSoFar bkt = taxSoFar |+| (getTaxCV bkt income')

-- Hand tuned CValued version since we call these a lot
getTaxCV' :: Currency -> ExchangeRateFunction -> TaxBracket -> CV.CVD -> CV.CVD
getTaxCV' ccy e (Bracket b t r) taxable'  =
  let tax' = CV.unwrap ccy e  taxable'
      MoneyValue b' _ = MV.convert b ccy e
      MoneyValue t' _ = MV.convert t ccy e
  in CV.fromMoneyValue $ MoneyValue (r * min (t' - b') (max 0 (tax' - b'))) ccy

getTaxCV' ccy e (TopBracket b r) taxable' =
  let tax' = CV.unwrap ccy e taxable'
      MoneyValue b' _ = MV.convert b ccy e
  in CV.toCV (r*max 0 (tax' - b')) ccy

computeIncomeTaxCV' :: Currency -> ExchangeRateFunction -> TaxBrackets -> CV.CVD -> CV.CVD
computeIncomeTaxCV' ccy e (TaxBrackets bkts) income' = foldl f (CV.mvZero ccy) bkts where
  f taxSoFar bkt = taxSoFar |+| (getTaxCV' ccy e bkt income')


getTaxCV'' :: Currency -> ExchangeRateFunction -> TaxBracket -> Double -> Double
getTaxCV'' ccy e (Bracket b t r) taxable'  =
  let MoneyValue b' _ = MV.convert b ccy e
      MoneyValue t' _ = MV.convert t ccy e
  in (r*min (t' - b') (max 0 (taxable' - b')))

getTaxCV'' ccy e (TopBracket b r) taxable' =
  let MoneyValue b' _ = MV.convert b ccy e
  in (r*max 0 (taxable' - b'))

computeIncomeTaxCV'' :: Currency -> ExchangeRateFunction -> TaxBrackets -> CV.CVD -> CV.CVD
computeIncomeTaxCV'' ccy e (TaxBrackets bkts) incomeCV =
  let income' = CV.unwrap ccy e incomeCV
      f taxSoFar bkt = taxSoFar + (getTaxCV'' ccy e bkt income')
      tax = foldl f 0 bkts
  in CV.fromMoneyValue $ MoneyValue tax ccy

marginalRateCV :: CV.CVD -> TaxBrackets -> CV.SVD
marginalRateCV v' (TaxBrackets its) = foldl f (CV.toSVD 0) its where
  f cmr (Bracket b _ r) = CV.cvIf (v' |>| CV.fromMoneyValue b) (CV.toSVD r) cmr
  f cmr (TopBracket b r) = CV.cvIf (v' |>| CV.fromMoneyValue b) (CV.toSVD r) cmr

trueMarginalRateCV :: MoneyValue -> TaxBrackets -> CV.SVD
trueMarginalRateCV mv@(MoneyValue _ c) tb =
  let one' = CV.toCV 1.0 c
      base' = CV.fromMoneyValue mv
      t1' = computeIncomeTaxCV tb base'
      t2' = computeIncomeTaxCV tb (base' CV.|+| one')
  in (t2' |-| t1') |/| one'



