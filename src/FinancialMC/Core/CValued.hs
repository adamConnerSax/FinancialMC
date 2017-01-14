{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module FinancialMC.Core.CValued
       (
         CVD
       , SVD
       , asERMV
       , asCERMV
       , fromMoneyValue
       , toMoneyValue
       , scZero
       , cvZero
       , mvZero
       , toCS
       , toSV
       , toSVD
       , toCV
       , toCVD
       , asERFReader
       , fixCCY
       , atCCY
       , atERF
       , unwrap
       , scBinOp
       , cvBinOp
       , scRFBinOp
       , cvRFBinOp
       , cvNegate
       , HasAddition(..)
       , HasMultiplication(..)
       , HasDivision(..)
       , HasEqOrdering(..)
       , HasMinMax(..)
       , cvCompare
       , cvIf
       , cvAnd
       , cvOr
       , cvCase
       ) where


import Control.Monad.Reader (MonadReader,ask,lift)

import FinancialMC.Core.MoneyValue (Currency(..),MoneyValue(..))
import FinancialMC.Core.MoneyValueOps (ERK,CR)
import FinancialMC.Core.CValued_Internal

type CVD = CValued Currency Double
type SVD = SValued Currency Double


toSV::a->SValued Currency a
toSV = toCS

toSVD::Double->SVD
toSVD = toSV

toCVD::(Currency->ERF Currency->Double)->CVD
toCVD = CVCV

fromMoneyValue::MoneyValue->CVD
fromMoneyValue (MoneyValue x c) = toCV x c
{-# INLINE fromMoneyValue #-}

toMoneyValue::Currency->ERF Currency->CVD->MoneyValue
toMoneyValue _ _ (CVMV x c _) = MoneyValue x c
toMoneyValue c e (CVCV g) = MoneyValue (g c e) c
{-# INLINE toMoneyValue #-}

asERMV::MonadReader (ERF Currency) m=>Currency->CVD->m MoneyValue 
asERMV c (CVCV g) = do
  e<-ask
  return $! MoneyValue (g c e) c
asERMV c (CVMV x c' f)
  | c == c'   = return $! MoneyValue x c
  | otherwise = do
      e <- ask
      return $! MoneyValue (f e c c' x) c


asCERMV::ERK m=>CVD->CR m MoneyValue
asCERMV a = ask >>= lift . flip asERMV a


-- This is here so that calculations using 0 and all in same ccy can stay in simple form.
mvZero::Currency->CVD
mvZero ccy = fromMoneyValue (MoneyValue 0 ccy)
--mvZero = const cvZero
