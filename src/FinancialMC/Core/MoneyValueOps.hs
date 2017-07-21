{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module FinancialMC.Core.MoneyValueOps
       (
         convert
       , FinancialMC.Core.MoneyValueOps.negate
       , isPositive
       , isNonNegative
       , isZero
       , multiply
       , divide
       , ratio
       , lt
       , gt
       , inFirst
       , binaryOp
       , unaryOp
       , total
       , zero
--       , ER
--       , CR
--       , ERK
       ) where


import           FinancialMC.Core.MoneyValue (Currency, ExchangeRateFunction,
                                              MoneyValue (MoneyValue))
import           Prelude                     hiding ((*>), (<*))

--import           Control.Monad.Reader        (MonadReader, ReaderT)
import           Control.Monad.State         (MonadState)
import           Data.List                   (foldl')

convert:: MoneyValue->Currency->ExchangeRateFunction->MoneyValue
convert mv@(MoneyValue x ccy') ccy e
  | ccy == ccy' = mv
  | otherwise = MoneyValue (x * e ccy ccy') ccy
{-# INLINABLE convert #-}

negate::MoneyValue->MoneyValue
negate (MoneyValue x ccy)  = MoneyValue (Prelude.negate x) ccy
{-# INLINABLE negate #-}

isPositive::MoneyValue->Bool
isPositive (MoneyValue x _) = x>0
{-# INLINABLE isPositive #-}

isNonNegative::MoneyValue->Bool
isNonNegative (MoneyValue x _) = x >= 0
{-# INLINABLE isNonNegative #-}

isZero::MoneyValue->Bool
isZero (MoneyValue x _) = abs x < 0.001
{-# INLINABLE isZero #-}

multiply::MoneyValue->Double->MoneyValue
multiply (MoneyValue x ccy) r = MoneyValue (r * x) ccy
{-# INLINABLE multiply #-}

divide::MoneyValue->Double->MoneyValue
divide (MoneyValue x ccy) r = MoneyValue (x/r) ccy
{-# INLINABLE divide #-}

ratio::ExchangeRateFunction->MoneyValue->MoneyValue->Double
ratio e = binaryOp e (/)
{-# INLINABLE ratio #-}

lt::ExchangeRateFunction->MoneyValue->MoneyValue->Bool
lt e  = binaryOp e (<)
{-# INLINABLE lt #-}

gt::ExchangeRateFunction->MoneyValue->MoneyValue->Bool
gt e  = binaryOp e (>)
{-# INLINABLE gt #-}

total::ExchangeRateFunction->Currency->[MoneyValue]->MoneyValue
total e ccy   = foldl' f  (zero ccy)  where
  f  = inFirst e (+)
{-# INLINABLE total #-}

zero::Currency->MoneyValue
zero = MoneyValue 0
{-# INLINABLE zero #-}

unaryOp::(Double->Double)->MoneyValue->MoneyValue
unaryOp f (MoneyValue x ccy) = MoneyValue (f x) ccy
{-# INLINABLE unaryOp #-}

inFirst::ExchangeRateFunction->(Double->Double->Double)->MoneyValue->MoneyValue->MoneyValue
inFirst e f (MoneyValue x' ccy) y = MoneyValue (f x' y') ccy where
  MoneyValue y' _ = convert y ccy e
{-# INLINABLE inFirst #-}

binaryOp::ExchangeRateFunction->(Double->Double->a)->MoneyValue->MoneyValue->a
binaryOp e f (MoneyValue x' ccy) y = let (MoneyValue y' _) = convert y ccy e in f x' y'
{-# INLINABLE binaryOp #-}

-- some types for Monad instances with Reader ExchangeRateFunction
--type ERK m = (MonadReader ExchangeRateFunction m)
--type ER s m = (MonadState s m, ReadsExchangeRateFunction s)
--type CR s m = (MonadState s m, ReadsCurrency s)

