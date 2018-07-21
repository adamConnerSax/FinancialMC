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
{-# LANGUAGE StandaloneDeriving #-}
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
--       , safeCapGainRateCV
       , safeCapGainRate
       , updateTaxRules
--       , fullTaxCV
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
import Data.Foldable (toList)
import qualified Safe
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
import Data.Maybe (fromMaybe)

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
data MedicareSurtax = MedicareSurtax { netInvRate :: Double, magiThreshold :: MoneyValue } deriving (Generic, Show, ToJSON, FromJSON)

fedCapitalGainsToFedCapitalGainsM :: FedCapitalGains -> TE.FedCapGainsM Double
fedCapitalGainsToFedCapitalGainsM (FedCapitalGains tr bands) = TE.FedCapGainsM tr (fmap capGainBandToCapGainBandM bands)

medicareSurtaxToMedicareSurtaxM :: ExchangeRateFunction -> Currency -> MedicareSurtax -> TE.MedicareSurtaxM Double
medicareSurtaxToMedicareSurtaxM erf c (MedicareSurtax nir mt) = TE.MedicareSurtaxM nir (moneyValueToMoney erf c mt)

newtype StandardDeductions = StandardDeductions { deds :: (A.Array TE.Jurisdiction MoneyValue) } deriving (Show)

instance ToJSON StandardDeductions where
  toJSON = toJSON . A.assocs . deds 

instance FromJSON StandardDeductions where
  parseJSON v =
    let f pairList = do
          (minIndex, _) <- Safe.headMay pairList
          (maxIndex, _) <- Safe.lastMay pairList
          return $ A.array (minIndex, maxIndex) pairList
    in StandardDeductions . (fromMaybe (A.listArray (minBound, maxBound) (repeat $ MV.zero USD))) . f <$> parseJSON v


data TaxRules = TaxRules
  {
    _trFederal :: !TaxBrackets,
    _trPayroll :: !TaxBrackets,
    _trEstate  :: !TaxBrackets,
    _trFCG     :: !FedCapitalGains,
    _trMedTax  :: !MedicareSurtax,
    _trState   :: !TaxBrackets,
    _trCity    :: !TaxBrackets,
    _trSDeds   :: !StandardDeductions,
    _trSALTCap :: Maybe MoneyValue
  } deriving (Show, Generic)

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
updateTaxRules  (TaxRules fed payroll estate fcg medstax st city sdeds scap) taxBracketInflationRate = newRules where
  fed' = inflateTaxBrackets taxBracketInflationRate fed
  payroll' = inflateTaxBrackets taxBracketInflationRate payroll
  estate' = inflateTaxBrackets taxBracketInflationRate estate
  st' = inflateTaxBrackets taxBracketInflationRate st
  city' = inflateTaxBrackets taxBracketInflationRate city
  sdeds' = StandardDeductions (flip MV.multiply (1.0 + taxBracketInflationRate) <$> deds sdeds)
  scap' = flip MV.multiply (1.0 + taxBracketInflationRate) <$> scap
  newRules = TaxRules fed' payroll' estate' fcg medstax st' city' sdeds' scap'

topBracketRate :: TaxBrackets -> Double
topBracketRate (TaxBrackets bs) =
  let rate (Bracket _ _ r) = r
      rate (TopBracket _ r) = r
  in foldl' (\cr bkt -> max cr (rate bkt)) 0 bs

-- required for tax trade rule.
-- If we need $x and we are selling something with capital gains, we need to know how much extra to trade
safeCapGainRate :: TaxRules -> Double
safeCapGainRate (TaxRules _ _ _ (FedCapitalGains tr _) _ state city _ _) = tr + topBracketRate state + topBracketRate city

taxRulesToTaxRulesM :: ExchangeRateFunction -> Currency -> TaxRules -> TE.TaxRulesM Double
taxRulesToTaxRulesM erf c (TaxRules fed payroll estate fcg ms state city sd scap) =
  let f = taxBracketsToTaxBracketsM erf c
      fedCGBrackets = TE.capGainBandsToBrackets (fedCapitalGainsToFedCapitalGainsM fcg) (f fed)
      brackets = A.array (minBound,maxBound)
        [
          (TE.Federal,f fed)
        , (TE.FedCG, fedCGBrackets)
        , (TE.State, f state)
        , (TE.Local, f city)
        , (TE.Payroll, f payroll)
        , (TE.Estate, f estate)
        ]
      sd' = moneyValueToMoney erf c <$> deds sd
      ms' = medicareSurtaxToMedicareSurtaxM erf c ms
  in TE.TaxRulesM brackets sd' (moneyValueToMoney erf c <$> scap) ms'


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
      (Money tax, rate) = TE.runTaxMonad (TE.taxReaderProgram basePolicy) (TE.TaxEnv trm tfs)
      taxMV = MoneyValue tax ccy
  return (taxMV, rate)

