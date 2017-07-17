{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module FinancialMC.Core.AssetTrading
       (
         nullAssetTradeF
       , defaultAssetBuySellF
       , defaultNonCapitalAssetBuySellF
       , liquidateOnlyBuySellF
       , tradeAccount
       ) where

import           FinancialMC.Core.Asset         (Account (..),
                                                 AssetRevaluation (..), IsAsset,
                                                 acAssets, accountValueCV,
                                                 assetCostBasis, assetCurrency,
                                                 assetValue, revalueAsset,
                                                 tradeAsset)
import           FinancialMC.Core.CValued       ((|*|), (|+|), (|-|), (|/|),
                                                 (|<|), (|>|))
import qualified FinancialMC.Core.CValued       as CV
import           FinancialMC.Core.Evolve        (FlowResult (..),
                                                 TaxAmount (..))
import           FinancialMC.Core.MoneyValue    (Currency (..), MoneyValue (..), ReadsExchangeRateFunction (getExchangeRateFunction),
                                                 mCurrency)
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Result        (adjust, appendAndReturn,
                                                 returnOnly)
import           FinancialMC.Core.Tax           (TaxType (CapitalGain, OrdinaryIncome))
import           FinancialMC.Core.TradingTypes  (AccountType (..),
                                                 LiquidateFunction, TradeAppC,
                                                 TradeFunction, TradeType (..))
import           Prelude                        hiding ((*>), (<*))


import           Control.Lens                   (set, (^.), use)
import           Control.Monad.Reader           (ask)
import           Data.Monoid                    ((<>))

--refuses all trades, current default for liabilities
-- gives back original asset and zero spent, zero cap gain
noTrade:: TradeAppC s m => a -> m a
noTrade = return
{-# INLINE noTrade #-}

type AssetTrader s m a = IsAsset a => TradeFunction s m a
type AssetLiquidator s m a = IsAsset a => LiquidateFunction s m a
type AccountTrader s m a = TradeAppC s m => Account a -> TradeType -> MoneyValue -> m (Account a)

--useful default for most assets
defaultAssetBuySellF :: AssetTrader s m a
defaultAssetBuySellF = buySellAssetHelper
{-# INLINE defaultAssetBuySellF #-}

buySellAssetHelper :: AssetTrader s m a
buySellAssetHelper target aTp tTp am = do
  e <- use getExchangeRateFunction
  let am' = CV.fromMoneyValue am
      target'  = CV.fromMoneyValue (assetValue target)
  CV.atERF e $ CV.cvCase
    [((am' |>| CV.mvZero (assetCurrency target)),CV.toCS $ buyAsset target aTp tTp am),
     ((CV.cvNegate am' |<| target'),CV.toCS $ sellAsset target aTp tTp (MV.negate am))]
    (CV.toCS $ liquidateAsset target aTp tTp)
{-# INLINE buySellAssetHelper #-}

nullAssetTradeF :: AssetTrader s m a
nullAssetTradeF a _ _ _ = return a

nonCapital :: [FlowResult] -> [FlowResult]
nonCapital flows = flows' where
  flows' = fmap noCapGain flows
  noCapGain (AllTaxed (TaxAmount CapitalGain x)) = UnTaxed x
  noCapGain (OnlyTaxed (TaxAmount CapitalGain x)) = UnTaxed (MV.zero (x^.mCurrency))
  noCapGain (PartiallyTaxed x (TaxAmount CapitalGain _)) = UnTaxed x
  noCapGain x = x

defaultNonCapitalAssetBuySellF :: AssetTrader s m a
defaultNonCapitalAssetBuySellF a aTp tTp am = adjust (return . nonCapital) $ defaultAssetBuySellF a aTp tTp am
{-# INLINE defaultNonCapitalAssetBuySellF #-}

getCapGain :: Currency -> [FlowResult] -> CV.CVD
getCapGain ccy flows = foldl (|+|) (CV.mvZero ccy) cgs where
  getCG (AllTaxed (TaxAmount CapitalGain x))         = x
  getCG (OnlyTaxed (TaxAmount CapitalGain x))        = x
  getCG (PartiallyTaxed _ (TaxAmount CapitalGain y)) = y
  getCG _                                            = MV.zero ccy
  cgs = fmap (CV.fromMoneyValue . getCG) flows


liquidateOnlyBuySellF :: AssetTrader s m a
liquidateOnlyBuySellF a aTp tTp am = do
  let ccy = assetCurrency a
      cgExempt = if (ccy == USD) && (aTp == PrimaryHome)
                 then CV.toCV 500000 USD
                 else CV.mvZero ccy

  let y flows = CV.asERMV ccy $ CV.cvMax (CV.mvZero ccy) ((getCapGain ccy flows) |-| cgExempt)
--      adjustF:: TradeAppC s m [FlowResult] => [FlowResult] -> m [FlowResult]
      adjustF flows = do
        adjCG <- y flows
        return $! flows <> [OnlyTaxed (TaxAmount CapitalGain adjCG)]

  a' <- if assetValue a == MV.negate am
         then adjust adjustF (liquidateAsset a aTp tTp)
         else noTrade a

  returnOnly a'
{-# INLINE liquidateOnlyBuySellF #-}

deductibleBuy :: TradeType -> AccountType -> Bool
deductibleBuy OverFund _ = False
deductibleBuy _ A401k    = True
deductibleBuy _ IRA      = True
deductibleBuy _ _        = False

buyAsset :: AssetTrader s m a
buyAsset a aTp tTp am = do
  let ccy = assetCurrency a
      am' = CV.fromMoneyValue am
  newAmount <- CV.asERMV ccy $ am' |+| CV.fromMoneyValue (assetValue a)
  newBasis  <- CV.asERMV ccy $ am' |+| CV.fromMoneyValue (assetCostBasis a)
  let a' = revalueAsset a (NewValueAndBasis newAmount newBasis)

      flow = if deductibleBuy tTp aTp
             then AllDeductible (TaxAmount OrdinaryIncome am)
             else UnTaxed (MV.negate am)

  appendAndReturn [flow] a'
{-# INLINE buyAsset #-}

liquidateAsset :: AssetLiquidator s m a
liquidateAsset a aTp tTp = sellAsset a aTp tTp (assetValue a)
{-# INLINE liquidateAsset #-}

retirementAccount :: AccountType -> Bool
retirementAccount A401k   = True
retirementAccount IRA     = True
retirementAccount RothIRA = True
retirementAccount _       = False

earlyRetirement :: AccountType -> TradeType -> Bool
earlyRetirement aTp EarlyWithdrawal | retirementAccount aTp = True
earlyRetirement _ _                 = False

early529 :: AccountType -> TradeType -> Bool
early529 A529 EarlyWithdrawal = True
early529 _ _                  = False

taxedRetirement :: AccountType -> Bool
taxedRetirement A401k = True
taxedRetirement IRA   = True
taxedRetirement _     = False

untaxed :: AccountType -> Bool
untaxed A529    = True
untaxed RothIRA = True
untaxed _       = False

proceedsF :: AccountType -> TradeType -> CV.CVD -> CV.CVD -> CV.CVD
proceedsF aTp tTp amount capGain
  | earlyRetirement aTp tTp = amount |*| (0.9 :: Double)
  | early529 aTp tTp = amount |-| (capGain |*| (0.1 :: Double))
  | otherwise = amount


sellAsset :: AssetTrader s m a
sellAsset a aTp tTp am = do
  let ccy = assetCurrency a
      av' = CV.fromMoneyValue (assetValue a)
      ab' = CV.fromMoneyValue (assetCostBasis a)
      am' = CV.fromMoneyValue am
      r' = ab' CV.|/| av'
      capGainCV = CV.cvIf (ab' |<| av') (am' |*| ((1.0::Double) |-| r'))  (CV.mvZero ccy)

  newBasis <- CV.asERMV ccy $ CV.cvIf (av' |>| (CV.mvZero ccy)) (r' |*| (av' |-| am')) (CV.mvZero ccy)
  newValue <- CV.asERMV ccy $ av' |-| am'
  proceeds <- CV.asERMV ccy $ proceedsF aTp tTp am' capGainCV
  capGain  <- CV.asERMV ccy capGainCV

  let a' = revalueAsset a (NewValueAndBasis newValue newBasis)
      flow
        | taxedRetirement aTp        = AllTaxed (TaxAmount OrdinaryIncome proceeds)
        | early529 aTp tTp           = PartiallyTaxed proceeds (TaxAmount OrdinaryIncome (MV.multiply capGain 0.9))
        | untaxed aTp                = UnTaxed proceeds
        | otherwise                  = PartiallyTaxed proceeds (TaxAmount CapitalGain capGain)

  appendAndReturn [flow] a'
{-# INLINE sellAsset #-}

tradeAccount :: IsAsset a => AccountTrader s m a
tradeAccount acct@(Account _ aTp ccy as) tTp am = do
  e <- use getExchangeRateFunction
  let av' = accountValueCV acct
      am' = CV.fromMoneyValue am
      recipN = 1.0/fromIntegral (length as)
      ratio a = CV.cvIf (av' |>| CV.mvZero ccy) ((CV.fromMoneyValue $ assetValue a) |/| av') (CV.toSVD recipN)
      bsAmt a = CV.toMoneyValue ccy e $ (ratio a) |*| am'
      amts = fmap bsAmt as
      trades =  zip as amts
  newAssets <- mapM (\(a,amt')->tradeAsset a aTp tTp amt') trades -- flows get added here
  returnOnly $ set acAssets newAssets acct
{-# INLINE tradeAccount #-}
