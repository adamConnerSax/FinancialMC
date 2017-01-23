{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
module FinancialMC.Builders.Assets (
  BaseAssetDetails(..)
  , BaseAsset(..)
  ) where

import Prelude hiding ((*>),(<*))

import FinancialMC.Core.Result (MonadResult(..))
import FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),Currency(USD))
import qualified FinancialMC.Core.MoneyValueOps as MV
import qualified FinancialMC.Core.CValued as CV
import           FinancialMC.Core.CValued ((|+|),(|-|),(|*|),(|/|))
import FinancialMC.Core.Evolve (Evolvable(..),Evolver,EvolveOutput(EvolveOutput),TaxAmount(..),FlowResult(..))
import FinancialMC.Core.Asset (IsAsset(..),assetCurrency,assetValue,assetCostBasis,AssetCore(..),revalueAssetCore,AssetRevaluation(..))
import FinancialMC.Core.AssetTrading (defaultNonCapitalAssetBuySellF,defaultAssetBuySellF,liquidateOnlyBuySellF,nullAssetTradeF)
import FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..))
import FinancialMC.Core.Rates (RateTable,rateRequest,RateTag(..),InterestType(..),ReturnType(..))
import FinancialMC.Core.Tax (TaxType(..))

import Control.Lens (magnify)
import Control.Monad.Reader (ReaderT,ask)
import Control.Monad.Trans (lift)
import Control.Exception (SomeException)

--import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (ToJSON(..),FromJSON(..),defaultOptions,fieldLabelModifier)

--import Data.Aeson.Existential (TypeNamed)

import GHC.Generics (Generic)

laERMV::Monad m=>Currency->CV.CVD->ReaderT (FinEnv rm) m MoneyValue
laERMV c = magnify feExchange . CV.asERMV c

assetCVMult::(IsAsset a,Monad m)=>a->Double->ReaderT (FinEnv rm) m MoneyValue
assetCVMult x rate = laERMV (assetCurrency x) $ rate CV.|*| CV.fromMoneyValue (assetValue x)

liftRates::ReaderT (RateTable Double) (Either SomeException) Double -> ReaderT (FinEnv rm) (Either SomeException) Double
liftRates = magnify feRates

data BaseAssetDetails =
  CashAsset |
  MixedFund !Double !Double !Double  | -- stockPct divYield bondInterest
  GuaranteedFund !Double  | -- rate
  ResidentialRE |
  FixedRateMortgage !Double !Int  {- rate years -} deriving (Generic,ToJSON,FromJSON)

{- $(deriveJSON defaultOptions ''FMCBaseAssetDetails) -}

instance Show BaseAssetDetails where
  show CashAsset = "Cash:"
  show (MixedFund pct sYld bInt) = "Mixed Stock/Bond Fund (" ++ show pct ++ " stock; div yld=" ++ show (100*sYld) 
                                      ++ "%; bond int=" ++ show (100*bInt) ++"%):"
  show (GuaranteedFund rate) = "Guaranteed return Fund (" ++ show (100*rate) ++ "% return):"
  show ResidentialRE = "Residential real estate:"
  -- following shouldn't be called since we special case it in FMCBaseAsset
  show (FixedRateMortgage rate years) = "Mortgage (Fixed " ++ show rate ++ " rate, "  
                                      ++ show years ++ " years):" 

data BaseAsset = BaseAsset !AssetCore !BaseAssetDetails deriving (Generic,ToJSON,FromJSON)

{- $(deriveJSON defaultOptions ''FMCBaseAsset) -}

instance Show BaseAsset where
  show (BaseAsset (AssetCore n v b) (FixedRateMortgage rate years)) =
    show n ++ " (Fixed " ++ show rate ++ " rate, " 
    ++ show years ++ " years mortgage): " 
    ++ show (MV.negate v) 
    ++ " owed of " ++ show (MV.negate b) ++ " borrowed."

  show (BaseAsset ac dets) = show dets ++ " " ++ show ac
                                        

instance IsAsset BaseAsset where
  assetCore (BaseAsset ac _) = ac
  revalueAsset (BaseAsset ac d) x = BaseAsset (revalueAssetCore ac x) d

  tradeAsset a@(BaseAsset _ CashAsset) = defaultNonCapitalAssetBuySellF a
  tradeAsset a@(BaseAsset _ ResidentialRE) = liquidateOnlyBuySellF a
  tradeAsset a@(BaseAsset _ (FixedRateMortgage _ _)) = nullAssetTradeF a
--  tradeAsset a@(BaseAsset _ (MixedFund _ _ _)) = defaultAssetBuySellF a
--  tradeAsset a@(BaseAsset _ (GuaranteedFund _)) = defaultAssetBuySellF
  tradeAsset a = defaultAssetBuySellF a


instance Evolvable BaseAsset where
  evolve a@(BaseAsset _ CashAsset) = cashEvolveFunction a
  evolve a@(BaseAsset _ (MixedFund _ _ _)) = mixedFundEvolveFunction a
  evolve a@(BaseAsset _ (GuaranteedFund _)) = guaranteedFundEvolveFunction a
  evolve a@(BaseAsset _ ResidentialRE) = residentialREEvolveF a
  evolve a@(BaseAsset _ (FixedRateMortgage _ _)) = fixedMortgageEvolveFunction a

-- these functions are almost all partial and definitely only meant to work on one asset detail type.
-- The Evolvable instance *is* total and these functions won't be exported
cashEvolveFunction::Evolver rm BaseAsset 
cashEvolveFunction ca = do
  (interest',v') <- lift $  do
    rate <- magnify feRates $ rateRequest (Interest Savings)
    interest <- assetCVMult ca rate
    v <- assetCVMult ca (1.0 + rate)
    return (interest,v)
  appendAndReturn (EvolveOutput [OnlyTaxed (TaxAmount NonPayrollIncome interest')] []) (revalueAsset ca (NewValueAndBasis v' v'))
  
mixedFundEvolveFunction::Evolver rm BaseAsset  
mixedFundEvolveFunction mf@(BaseAsset _ (MixedFund fracStock stkYield bondInterest)) = do
  (flows', newA') <- lift $ do
    stockRet <- liftRates $ rateRequest (Return Stock)
    bondRet <- liftRates $ rateRequest (Return Bond)
    let ccy = assetCurrency mf
        oldV' = CV.fromMoneyValue (assetValue mf)
        stockDivs' = oldV' |*| (fracStock*stkYield*(1+((stockRet-stkYield)/2.0)))
        bondDivs'  = oldV' |*| ((1-fracStock)*bondInterest*(1+((bondRet-bondInterest)/2.0)))
    stockDivs <- laERMV ccy stockDivs' 
    bondDivs  <- laERMV ccy bondDivs'
    let flows =  [OnlyTaxed (TaxAmount Dividend stockDivs), OnlyTaxed (TaxAmount NonPayrollIncome bondDivs)]  
        rate = fracStock*stockRet + (1.0-fracStock)*bondRet    
    newV <- assetCVMult mf (1.0 + rate) 
    newB <- laERMV ccy $  CV.fromMoneyValue (assetCostBasis mf) |+| stockDivs' |+|  bondDivs'
    return (flows, revalueAsset mf (NewValueAndBasis newV newB))    
  appendAndReturn (EvolveOutput flows' []) newA'  

guaranteedFundEvolveFunction::Evolver rm BaseAsset
guaranteedFundEvolveFunction gf@(BaseAsset _ (GuaranteedFund rate)) = do
  v' <- lift $ assetCVMult gf (1.0 + rate)
  returnOnly $! revalueAsset gf (NewValue v')

residentialREEvolveF::Evolver rm BaseAsset
residentialREEvolveF rre = do
  v <- lift $ do 
    ret <- liftRates $ rateRequest (Return RealEstate)
    assetCVMult rre (1.0 + ret)
  returnOnly $! revalueAsset rre $ NewValue v


fixedMortgageEvolveFunction::Evolver rm BaseAsset
fixedMortgageEvolveFunction frm@(BaseAsset _ (FixedRateMortgage rate years)) =  do
  let ccy = (assetCurrency frm)     
      borrowed' = CV.cvNegate $ CV.fromMoneyValue (assetCostBasis frm)
      curPrincipal' = CV.cvNegate $ CV.fromMoneyValue (assetValue frm)  
      r = rate/fromIntegral (12::Int) 
      n = (12::Int)*years
      x = (1+r)^n
      monthly' = borrowed' |*| (r*x/(x-1)) --amortization formula
  (d,np,p)<- lift . magnify feExchange $ do
        (newPrincipal',paid') <- doPayments ccy monthly' r curPrincipal' (12::Int)
        let interest' = paid' |-| (curPrincipal' |-| newPrincipal')
            deductible_fraction' = CV.cvMin (CV.toSVD 1.0) ((CV.toCV 1000000 USD) |/| curPrincipal') 
        deduction <- CV.asERMV ccy (deductible_fraction' |*| interest')
        newPrincipal <- CV.asERMV ccy newPrincipal'
        paid <- CV.asERMV ccy paid'
        return (deduction, newPrincipal, paid)
  
  let newAsset = revalueAsset frm (NewValue $ MV.negate np) 
      cashFlow = PartiallyDeductible (MV.negate p) (TaxAmount OrdinaryIncome d)
  appendAndReturn (EvolveOutput [cashFlow] []) newAsset


doPayments::MV.ERK m=>Currency->CV.CVD->Double->CV.CVD->Int->m (CV.CVD,CV.CVD)
doPayments ccy pmt' rate prin' n = do
  e <- ask
  let pmt = CV.unwrap ccy e pmt'
      prin  = CV.unwrap ccy e prin'
      SD pRemaining aPaid = foldl (\a _ -> doPayment pmt rate a) (SD prin 0) [1..n]
  return (CV.fromMoneyValue (MoneyValue pRemaining ccy),CV.fromMoneyValue (MoneyValue aPaid ccy))


data SD = SD !Double !Double
doPayment::Double->Double->SD->SD
doPayment pmt rate (SD prin paidSoFar) =
  let int = rate * prin
      pRemaining = max 0 (prin + int - pmt)
      pPaid = prin - pRemaining
      aPaid = int + pPaid + paidSoFar
  in SD pRemaining aPaid

doPayments''::Currency->CV.CVD->Double->CV.CVD->Int->(CV.CVD,CV.CVD) 
doPayments'' ccy pmt rate prin n = foldl (\a _ -> doPayment'' ccy pmt rate a) (prin,(CV.mvZero ccy)) [1..n] 
  
doPayment''::Currency->CV.CVD->Double->(CV.CVD,CV.CVD)->(CV.CVD,CV.CVD)
doPayment'' ccy pmt' rate (prin',paidSoFar') = 
  let interest' = rate |*| prin'
      prinRemaining' = CV.cvMax (CV.mvZero ccy) (prin' |+| interest' |-| pmt')
      prinPaid' = prin' |-| prinRemaining'
      amtPaid' = interest' |+| prinPaid' |+| paidSoFar'
  in (prinRemaining',amtPaid') 


{-
data CashAsset = CashAsset { caCore:: !AssetCore } deriving (Generic)

instance Show CashAsset where
  show (CashAsset ac) = "Cash: " ++ show ac


instance TypeNamed CashAsset 

instance IsAsset CashAsset where
  assetCore (CashAsset ac) = ac
  tradeAsset = defaultNonCapitalAssetBuySellF  
  revalueAsset (CashAsset ac) x = CashAsset (revalueAssetCore ac x)
  
$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''CashAsset)  

  
data MixedFund = MixedFund {mfCore:: !AssetCore, mfStockPct:: !Double, mfDividendYield:: !Double, mfBondInterest:: !Double} deriving (Generic)
                 
                 
instance Show MixedFund where
  show (MixedFund ac pct sYld bInt) = "Mixed Stock/Bond Fund (" ++ show pct ++ " stock; div yld=" ++ show (100*sYld) 
                                      ++ "%; bond int=" ++ show (100*bInt) ++"%):" ++ show ac
                               
instance Evolvable MixedFund where
  evolve = mixedFundEvolveFunction  

instance TypeNamed MixedFund

instance IsAsset MixedFund where
  assetCore (MixedFund ac _ _ _) = ac
  tradeAsset = defaultAssetBuySellF  
  revalueAsset (MixedFund ac pct sYld bInt) x = MixedFund (revalueAssetCore ac x) pct sYld bInt

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''MixedFund)

data GuaranteedFund = GuaranteedFund {gfCore:: !AssetCore, gfRate:: !Double} deriving (Generic)

                 
instance Show GuaranteedFund where
  show (GuaranteedFund ac rate) = "Guaranteed return Fund (" ++ show (100*rate) ++ "% return):" ++ show ac
                               
instance Evolvable GuaranteedFund where
  evolve = guaranteedFundEvolveFunction  

instance TypeNamed GuaranteedFund 

instance IsAsset GuaranteedFund where
  assetCore (GuaranteedFund ac _) = ac
  tradeAsset = defaultAssetBuySellF  
  revalueAsset (GuaranteedFund ac pct) x = GuaranteedFund (revalueAssetCore ac x) pct

$(deriveJSON defaultOptions{fieldLabelModifier= drop 2} ''GuaranteedFund)                   


data ResidentialRE = ResidentialRE { rreCore:: !AssetCore } deriving (Generic)

instance Show ResidentialRE where
  show (ResidentialRE ac) = "Residential real estate: " ++ show ac
  
instance Evolvable ResidentialRE where
  evolve = residentialREEvolveF
  
instance TypeNamed ResidentialRE 

instance IsAsset ResidentialRE where
  assetCore (ResidentialRE ac) = ac
  tradeAsset = liquidateOnlyBuySellF
  revalueAsset (ResidentialRE ac) x = ResidentialRE (revalueAssetCore ac x) 

$(deriveJSON defaultOptions{fieldLabelModifier= drop 3} ''ResidentialRE)  


data FixedRateMortgage = FixedRateMortgage { frmCore:: !AssetCore,
                                             frmRate:: !Double,
                                             frmYears:: !Int } deriving (Generic)
  

instance Show FixedRateMortgage where
  show frm@(FixedRateMortgage _ r yrs) = show (assetName frm) ++ " (Fixed " ++ show r ++ " rate, " 
                                                   ++ show yrs ++ " years mortgage): " 
                                                   ++ show (MV.negate $ assetValue frm) 
                                                   ++ " owed of " ++ show (MV.negate $ assetCostBasis frm) ++ " borrowed." 
                                                   
instance Evolvable FixedRateMortgage where
  evolve  = fixedMortgageEvolveFunction
    

instance TypeNamed FixedRateMortgage 


instance IsAsset FixedRateMortgage where
  assetCore (FixedRateMortgage ac _ _) = ac
  tradeAsset = nullAssetTradeF
  revalueAsset (FixedRateMortgage ac r y) x = FixedRateMortgage (revalueAssetCore ac x) r y 


$(deriveJSON defaultOptions{fieldLabelModifier= drop 3} ''FixedRateMortgage)

-}

