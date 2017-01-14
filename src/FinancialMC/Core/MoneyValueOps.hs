{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE ConstraintKinds #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE UndecidableInstances #-} 

module FinancialMC.Core.MoneyValueOps 
       (
         convert,
         FinancialMC.Core.MoneyValueOps.negate,
         isPositive,
         isNonNegative,
         isZero,
         multiply,
         divide,
         ratio,
         lt,
         gt,
         inFirst,
         binaryOp,
         unaryOp,
         total,
         zero,
         ER,CR,ERK
--       , ED, AERK, CER
       ) where


import Prelude hiding ((*>),(<*))
import FinancialMC.Core.MoneyValue (MoneyValue(MoneyValue),Currency,ExchangeRateFunction)

import Control.Monad.Reader (ReaderT,MonadReader)
import Data.List (foldl')

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
type ERK m = (MonadReader ExchangeRateFunction m)
type ER m = ReaderT ExchangeRateFunction m 
type CR m = ReaderT Currency m

{-
type CER m = CR (ER m)
type ED m = m Double
type AERK m = (ERK m, Applicative m)
-}
{-
function based instances to simplify math.  We need the concrete version for read/show but the below is much easier to compute with
--using applicatives, can compute with CER m Double using plain math ops
--then make them ER m MoneyValue using "fixC" and then those can be used applicatively to build up results in any stack
--with ReaderT ExchangeRateFunction 
-}

-- Comment out from here down??
{-
type CEMV m = CR m MoneyValue


runC::ERK m=>CR m a->Currency->m a
runC  = runReaderT 

fixC::ERK m=>Currency->CR m a->m a
fixC = flip runC

mv2cer::ERK m=>MoneyValue->CEMV m
mv2cer mv@(MoneyValue x c) = do
  c' <- ask
  let diffC = do
        e <- lift ask
        return $! MoneyValue (e c' c * x) c' 
  if c == c' then return $! mv else diffC
{-# INLINEABLE mv2cer #-}

d2cer::ERK m=>Double->CEMV m
d2cer x =  do
  c <- ask
  return $! MoneyValue x c
  
  
cer0::ERK m=>CEMV m
cer0 = d2cer 0


liftedBinOp::ERK m=>(Double->Double->Double)->MoneyValue->MoneyValue->CEMV m
liftedBinOp f (MoneyValue x cx) (MoneyValue y cy) = do
  c <- ask
  let diffC = do
        e <- lift ask
        return $! MoneyValue (f (e c cx * x) (e c cy * y)) c 
      sameC = return $! MoneyValue (f x y) c
  if c==cx && c==cy then sameC else diffC
{-# INLINEABLE liftedBinOp #-}


binOpHelper::ERK m=>(Double->Double->a)->CEMV m->CEMV m->m a
binOpHelper f mx my = stripCcy $ do
  e<-lift ask
  liftM2 (binaryOp e f) mx my
{-# INLINEABLE binOpHelper #-}

class AGroup a where                   
  (|+|)::a->a->a
  neg::a->a
  (|-|)::a->a->a
  x |-| y = x |+| neg y

class MMult v k | v->k where
  (*>)::k->v->v
  (<*)::v->k->v
  (/>)::k->v->v
  (</)::v->k->v
  
class MDiv v k | v->k where
  (|/|)::v->v->k
  
instance Applicative m=>AGroup (m Double) where 
  x |+| y = (+) <$> x <*> y
  neg x = Prelude.negate <$> x
                   
cemvBinOp::AERK m=>(Double->Double->Double)->CEMV m->CEMV m->CEMV m          
cemvBinOp f x y = do
  MoneyValue x' cx <- x 
  MoneyValue y' cy <- y 
  c <- ask
  e <- lift ask
  return $!  MoneyValue (f (e c cx * x') (e c cy * y')) c  
{-# INLINEABLE cemvBinOp #-}

{-  let sameC = return $ MoneyValue (f x' y') c
      diffC = do
        e <- lift ask
        return $  MoneyValue (f (e c cx * x') (e c cy * y')) c  
  if cx == c && cy == c then sameC else diffC
-}
  
instance AERK m=>AGroup (CEMV m) where
  x |+| y = cemvBinOp (+) x y
  x |-| y = cemvBinOp (-) x y 
  neg = liftM FinancialMC.Core.MoneyValueOps.negate
  
instance AERK m=>MMult (CR m MoneyValue) (m Double) where
  k *> x = liftM2 multiply x (lift k)
  x <* k = k *> x
  k /> x =  liftM2 divide x (lift k)
  x </ k = k /> x
  
stripCcy::ERK m=>CR m a->m a  
stripCcy = fixC USD  -- ICK

instance AERK m=>MDiv (CR m MoneyValue) (m Double) where
  x |/| y = stripCcy $ do 
   e <- lift ask
   MoneyValue x' cx <- x
   MoneyValue y' cy <- y    
   return $! e cy cx * x' / y'

(*|)::AERK m=>Double->CEMV m->CEMV m
(*|) k x = (return $! k) *> x

(|*)::AERK m=>CEMV m->Double->CEMV m
(|*) x k = x <* (return $! k)

(/|)::AERK m=>Double->CEMV m->CEMV m
(/|) k x = (return $! k) /> x

(|/)::AERK m=>CEMV m->Double->CEMV m
(|/) x k = x </ (return $! k)

  
mvEQ,mvNEQ,mvGT,mvLT,mvGTE,mvLTE::AERK m=>CEMV m->CEMV m->m Bool
mvEQ = binOpHelper (==)
mvNEQ = binOpHelper (/=)
mvGT = binOpHelper (>)
mvLT = binOpHelper (<)
mvGTE = binOpHelper (>=)
mvLTE = binOpHelper (<=)

(|==|),(|/=|),(|>|),(|<|),(|>=|),(|<=|)::AERK m=>CEMV m->CEMV m->m Bool
(|==|) = mvEQ
(|/=|) = mvNEQ
(|>|) = mvGT
(|<|) = mvLT
(|>=|) = mvGTE
(|<=|) = mvLTE


mCompare::AERK m=>ED m->ED m->m Ordering
mCompare x y = compare <$>  x <*>  y

mIf'::Monad m=>m Bool->CR m a->CR m a->CR m a
mIf' cond thenX elseX = do
  cond' <- lift cond
  if cond' then thenX else elseX
  
mIf::Monad m=>m Bool->m a->m a->m a
mIf cond thenX elseX = do
  cond' <- cond
  if cond' then thenX else elseX

mChoose::Monad m=>m Bool->a->a->m a
mChoose cond thenX elseX = do
  cond' <- cond
  return $! if cond' then thenX else elseX 


mOr::Monad m=>m Bool->m Bool->m Bool
mOr ma mb = do
  a <- ma
  b <- mb
  return $! (a || b)
  
mAnd::Monad m=>CR m Bool->CR m Bool->CR m Bool
mAnd ma mb = do
  a <- ma
  b <- mb
  return $! (a && b)

mCase::Monad m=>[(m Bool,m a)]->m a->m a
mCase cases defaultAction = do
  let  f (foundYet,currentAction) (cond,action) = 
        if foundYet 
        then return (foundYet,currentAction) 
        else cond >>= (\b -> if b then return (True,action) else return (False,currentAction))
  result<-foldM f (False,defaultAction) cases        
  snd result

class MOrd m a where
  mMin::m a->m a->m a
  mMax::m a->m a->m a
  
cemvOrdHelper::AERK m=>(Double->Double->Bool)->CEMV m->CEMV m->CEMV m  
cemvOrdHelper comp x y = do  
  MoneyValue x' cx <- x 
  MoneyValue y' cy <- y
  e <- lift ask
  if (e cy cx * x') `comp` y' then x else y
{-
  let diffC = do
        e <- lift ask
        if (e cy cx * x') `comp` y' then x else y
      sameC = if x' `comp` y' then x else y
  if cx==cy then sameC else diffC
-}

instance AERK m=>MOrd (CR m) MoneyValue where
  mMin = cemvOrdHelper (<)    
  mMax = cemvOrdHelper (>)    

instance AERK m=>MOrd m Double where
  mMin x y = min <$>  x <*>  y
  mMax x y = max <$>  x <*>  y 

-}              
