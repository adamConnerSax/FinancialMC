{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module FinancialMC.Core.CValued_Internal
       (
         SValued (..)
       , CValued (..)
       , HasOneValue(..)
       , ERF
       , scZero
       , cvZero
       , toCS
       , toCV
       , asERFReader
       , fixCCY
       , atCCY
       , atERF
       , unwrap
       , scBinOp
       , cvBinOp
       , scRFBinOp
       , cvRFBinOp
       , scNegate
       , cvNegate
       , HasAddition(..)
       , HasMultiplication(..)
       , HasDivision(..)
       , HasEqOrdering(..)
       , HasMinMax(..)
       , HasCase(..)
       , HasAndOr(..)
       , HasIf(..)
       , cvCompare
       ) where
       

import Control.Monad.Reader (MonadReader,ask)
import Control.DeepSeq (NFData(rnf))

import FinancialMC.Core.MoneyValue (Currency(USD))

{- 
This is sort of icky but required in the cases where we need a ccy for evaluation though the answer doesn't depend on it
I would like this to be unneccessary by construction somehow.
--} 
class HasOneValue a where
  oneValue::a

instance HasOneValue Currency where
  oneValue = USD

  
type Conv c a = ERF c->c->c->a->a

type ERF c = c->c->Double

data SValued c a = CVS !a | CVEV (ERF c -> a)
data CValued c a = CVCV (c -> ERF c -> a) | CVMV !a !c (Conv c a)

instance NFData a=>NFData (SValued c a) where
  rnf (CVS a) = rnf a `seq` ()
  rnf (CVEV _) = undefined

instance (NFData a, NFData c)=>NFData (CValued c a) where
  rnf (CVMV a c _) = rnf a `seq` rnf c `seq` ()
  rnf (CVCV _) = undefined

simpleConv::RealFrac a=>Conv c a
simpleConv e c1 c2 = (*) (realToFrac  $ e c1 c2)
{-# INLINABLE simpleConv #-}

optConv::Eq c=>Conv c a->Conv c a
optConv con e c1 c2 x
  | c1 == c2 = x
  | otherwise = con e c1 c2 x
{-# INLINABLE optConv #-}

conv::(Eq c,RealFrac a) => Conv c a
conv = optConv simpleConv
{-# INLINABLE conv #-}

scZero::RealFrac a=>SValued c a
scZero = CVS 0
{-# INLINABLE scZero #-}

cvZero::RealFrac a=>CValued c a
cvZero = CVCV $ const (const 0)
{-# INLINABLE cvZero #-}

instance Functor (SValued c) where
  fmap h (CVS x) = CVS $ h x
  fmap h (CVEV f) = CVEV $ h . f  
  {-# INLINABLE fmap #-}

instance Functor (CValued c) where
  fmap h (CVCV g) = CVCV (\c' e -> h (g c' e))
  fmap h (CVMV x c f) = CVCV (\c' e -> h (f e c' c x))
  {-# INLINABLE fmap #-}

instance Applicative (SValued c) where
  pure = CVS 
  {-# INLINABLE pure #-}
  (CVS f) <*> (CVS a) = CVS (f a)
  (CVS f) <*> (CVEV fa) = CVEV $ \e -> f (fa e)
  (CVEV ff) <*> (CVS a) = CVEV $ \e -> ff e a
  (CVEV ff) <*> (CVEV fa) = CVEV $ \e -> ff e (fa e)
  {-# INLINABLE (<*>) #-}
                      
instance Applicative (CValued c) where
  pure x = CVCV (\_ _ -> x)
  {-# INLINABLE pure #-}                                        
  (CVCV gf) <*> (CVCV ga) = CVCV (\c e -> gf c e (ga c e)) 
  (CVCV gf) <*> (CVMV x c h) = CVCV (\c' e -> gf c' e (h e c' c x))
  (CVMV f c h) <*> (CVCV ga) = CVCV (\c' e -> h e c' c f (ga c' e))
  (CVMV f cf hf) <*> (CVMV a ca ha) = CVCV (\c' e -> (hf e c' cf f) (ha e c' ca a))
  {-# INLINABLE (<*>) #-}

instance Monad (SValued c) where
  return = pure
  {-# INLINABLE return #-}
  CVS x >>= f = f x 
  (CVEV fa) >>= f = CVEV $ \e -> let CVEV q = f (fa e) in q e
  {-# INLINABLE (>>=) #-}

                
instance Monad (CValued c) where
  return = pure
  {-# INLINABLE return #-}                                                   
  (CVCV ga) >>= f = CVCV $ \c' e -> case f (ga c' e) of
                                       CVCV q ->  q c' e
                                       CVMV x c h -> h e c' c x

  (CVMV a c h) >>= f = CVCV $ \c' e -> case f (h e c' c a) of
                                      CVCV q -> q c' e
                                      CVMV a' c'' h' -> h' e c' c'' a'
  {-# INLINABLE (>>=) #-}                    

instance Show a => Show (SValued c a) where
  show (CVS x) = "scalar: " ++ show x
  show (CVEV _) = "CValued (abstract scalar)"                 

instance (Show a, Show c) => Show (CValued c a) where
  show (CVCV _)  = "CValued (abstract currency valued)"
  show (CVMV x c _) = show x ++ " " ++ show c 

toCS::a->SValued c a
toCS = CVS
{-# INLINABLE toCS #-}

toCV::Eq c=>Double->c->CValued c Double
toCV x c = CVMV x c conv
{-# INLINABLE toCV #-}

asCVMV::(RealFrac a,Eq c)=>c->ERF c->CValued c a->CValued c a
asCVMV _ _ mv@(CVMV _ _ _) = mv 
asCVMV c e (CVCV g) = CVMV (g c e) c conv
{-# INLINABLE asCVMV #-}

asERFReader::MonadReader (ERF c) m=>SValued c a -> m a
asERFReader (CVS x) = return x
asERFReader (CVEV f) = do
  e <- ask
  return $! f e
{-# INLINABLE asERFReader #-}

fixCCY::Eq c=>MonadReader (ERF c) m=>c->CValued c a->m a
fixCCY ccy  = asERFReader . atCCY ccy 
{-# INLINABLE fixCCY #-}

atCCY::Eq c=>c -> CValued c a -> SValued c a
atCCY c (CVCV f) = CVEV (f c)
atCCY c' (CVMV x c f) 
  | c == c' = CVS x
  | otherwise = CVEV (\e -> f e c' c x)
{-# INLINABLE atCCY #-}

atERF::ERF c->SValued c a->a
atERF _ (CVS x) = x
atERF e (CVEV f) = f e
{-# INLINABLE atERF #-}

unwrap::Eq c=>c -> ERF c -> CValued c a -> a
unwrap c e = (atERF e) . (atCCY c)
{-# INLINABLE unwrap #-}

scBinOp::(a->b->z)->SValued c a -> SValued c b -> SValued c z
scBinOp op (CVS x) (CVS y) = CVS (op x y)
scBinOp op (CVS x) (CVEV fy) = CVEV $ \e -> op x (fy e)
scBinOp op (CVEV fx) (CVS y) = CVEV $ \e -> op (fx e) y
scBinOp op (CVEV fx) (CVEV fy) = CVEV $ \e -> op (fx e) (fy e)
{-# INLINABLE scBinOp #-}


cvBinOp::(a->b->z)->CValued c a -> CValued c b -> CValued c z
cvBinOp op (CVCV gx) (CVCV gy) = CVCV $ \c e -> op (gx c e) (gy c e)
cvBinOp op (CVCV gx) (CVMV y cy hy) = CVCV $ \c e -> op (gx c e) (hy e c cy y)
cvBinOp op (CVMV x cx hx) (CVCV gy) = CVCV $ \c e -> op (hx e c cx x) (gy c e)
cvBinOp op (CVMV x cx hx) (CVMV y cy hy) = CVCV $ \c e -> op (hx e c cx x) (hy e c cy y)
{-# INLINABLE cvBinOp #-}

scRFBinOp::(a->a->b)->SValued c a -> SValued c a -> SValued c b
scRFBinOp  = scBinOp
{-# INLINABLE scRFBinOp #-}


cvRFBinOp::(Eq c, RealFrac b)=>(a->a->b)->CValued c a -> CValued c a -> CValued c b
cvRFBinOp op a@(CVMV x1 c1 _) b@(CVMV x2 c2 _) 
  | c1 == c2 = CVMV (op x1 x2) c1 conv
  | otherwise = cvBinOp op a b
cvRFBinOp op a b = cvBinOp op a b
{-# INLINABLE cvRFBinOp #-}
{-# SPECIALIZE cvRFBinOp::(Double->Double->Double)->CValued Currency Double->CValued Currency Double->CValued Currency Double #-}

--currency independent binary ops
ciBinOp::(Eq c, HasOneValue c)=>(a->a->b)->CValued c a->CValued c a->SValued c b
ciBinOp op (CVCV gx) (CVCV gy) = CVEV $ \e -> op (gx oneValue e) (gy oneValue e)
ciBinOp op (CVCV gx) (CVMV y cy _) = CVEV $ \e -> op (gx cy e) y
ciBinOp op (CVMV x cx _) (CVCV gy) = CVEV $ \e -> op x (gy cx e)
ciBinOp op (CVMV x cx hx) (CVMV y cy _)
  | cx ==cy = CVS $ op x y
  | otherwise = CVEV $ \e -> op (hx e cy cx x) y
{-# INLINABLE ciBinOp #-}
{-# SPECIALIZE ciBinOp::(Double->Double->Double)->CValued Currency Double->CValued Currency Double->SValued Currency Double #-}

scNegate::Num a=>SValued c a->SValued c a
scNegate x = negate <$> x 
{-# INLINABLE scNegate #-}

cvNegate::Num a=>CValued c a->CValued c a
cvNegate x = negate <$> x 
{-# INLINABLE cvNegate #-}

type family SumT (a :: *) (b :: *) :: * where
  SumT (CValued c a) (CValued c a) = CValued c a
  SumT (SValued c a) (SValued c a) = SValued c a
  SumT a (SValued c a) = SValued c a -- these overlap but are okay since type family is closed and RHS are the same (?)
  SumT (SValued c a) a = SValued c a

class (SumT a b ~ c) => HasAddition a b c where
  (|+|)::a->b->c
  (|-|)::a->b->c

instance (RealFrac a,q ~ SValued c a, SumT q q ~ q)=>HasAddition (SValued c a) (SValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasAddition (SValued Currency Double) (SValued Currency Double) (SValued Currency Double) #-}
  (|+|) = scRFBinOp (+)
  {-# INLINABLE (|+|) #-}
  (|-|) = scRFBinOp (-)
  {-# INLINABLE (|-|) #-}

instance (Eq c, RealFrac a, q ~ CValued c a, SumT q q ~ q )=>HasAddition (CValued c a) (CValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasAddition (CValued Currency Double) (CValued Currency Double) (CValued Currency Double) #-}
  (|+|) = cvRFBinOp (+)
  {-# INLINABLE (|+|) #-}
  (|-|) = cvRFBinOp (-)
  {-# INLINABLE (|-|) #-}

instance (Num a, q ~ SValued c a, SumT a q ~ q)=>HasAddition a (SValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasAddition Double (SValued Currency Double) (SValued Currency Double) #-}
  x |+| (CVS y) = CVS (x+y)
  x |+| (CVEV f) = CVEV $ \e->x + f e
  {-# INLINABLE (|+|) #-}
  x |-| (CVS y) = CVS (x-y)
  x |-| (CVEV f) = CVEV $ \e->x - f e
  {-# INLINABLE (|-|) #-}

instance (Num a, q ~ SValued c a, SumT q a ~ q)=>HasAddition (SValued c a) a (SValued c a) where
  {-# SPECIALIZE instance HasAddition (SValued Currency Double) Double (SValued Currency Double) #-}
  (CVS y) |+| x = CVS (y+x)
  (CVEV f) |+| x = CVEV $ \e->f e + x
  {-# INLINABLE (|+|) #-}
  (CVS y) |-| x = CVS (x-y)
  (CVEV f) |-| x = CVEV $ \e->x - f e 
  {-# INLINABLE (|-|) #-}


type family ProdT (a :: *) (b :: *) :: * where
  ProdT (SValued c a) (SValued c a) = SValued c a
  ProdT (SValued c a) (CValued c a) = CValued c a
  ProdT (CValued c a) (SValued c a) = CValued c a
  ProdT (SValued c a) a             = SValued c a
  ProdT a             (SValued c a) = SValued c a
  ProdT (CValued c a) a             = CValued c a
  ProdT a             (CValued c a) = CValued c a

class (c ~ ProdT a b) => HasMultiplication a b c where
    (|*|)::a->b->c

instance (RealFrac a, q ~ SValued c a, ProdT q q ~ q)=>HasMultiplication (SValued c a) (SValued c a) (SValued c a) where
    {-# SPECIALIZE instance HasMultiplication (SValued Currency Double) (SValued Currency Double) (SValued Currency Double) #-}
    (|*|) = scRFBinOp (*)
    {-# INLINABLE (|*|) #-}
                                                                                    
instance (RealFrac a, q ~ CValued c a, ProdT (SValued c a) q ~ q)=>HasMultiplication (SValued c a) (CValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasMultiplication (SValued Currency Double) (CValued Currency Double) (CValued Currency Double) #-}
  (CVS y)  |*| (CVCV g) = CVCV $ \c e -> y * (g c e)                                                                         
  (CVEV f) |*| (CVCV g) = CVCV $ \c e -> (f e) * (g c e)
  (CVS y)  |*| (CVMV x c h) = CVMV (y*x) c h
  (CVEV f) |*| (CVMV x c h) = CVCV $ \c' e -> (f e) * (h e c' c x)
  {-# INLINABLE (|*|) #-}

instance (RealFrac a, q ~ CValued c a, ProdT q (SValued c a) ~ q)=>HasMultiplication (CValued c a) (SValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasMultiplication (CValued Currency Double) (SValued Currency Double) (CValued Currency Double) #-}
  cv |*| sc = sc |*| cv                                                                        
  {-# INLINABLE (|*|) #-}

instance (RealFrac a, q ~ SValued c a, ProdT a q ~ q)=>HasMultiplication  a (SValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasMultiplication Double (SValued Currency Double) (SValued Currency Double) #-}
  x |*| (CVS y)  = CVS (x*y) 
  x |*| (CVEV f) = CVEV $ \e -> x * f e
  {-# INLINABLE (|*|) #-}

instance (RealFrac a, q ~ CValued c a, ProdT a q ~ q)=>HasMultiplication  a (CValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasMultiplication Double (CValued Currency Double) (CValued Currency Double) #-}
  x |*| (CVCV g) = CVCV (\c' e -> x * g c' e) 
  x |*| (CVMV y c f) = CVMV (x*y) c f 
  {-# INLINABLE (|*|) #-}

instance (RealFrac a, q ~ SValued c a, ProdT q a ~ q, ProdT a q ~ q)=>HasMultiplication (SValued c a) a (SValued c a) where
  {-# SPECIALIZE instance HasMultiplication (SValued Currency Double) Double (SValued Currency Double) #-}
  a |*| x = x |*| a
  {-# INLINABLE (|*|) #-}

instance (RealFrac a, q ~ CValued c a, ProdT q a ~ q, ProdT a q ~ q)=>HasMultiplication (CValued c a) a (CValued c a) where
  {-# SPECIALIZE instance HasMultiplication (CValued Currency Double) Double (CValued Currency Double) #-}
  a |*| x = x |*| a 
  {-# INLINABLE (|*|) #-}


type family QuotT (a :: *) (b :: *) :: * where
  QuotT (SValued c a) (SValued c a) = SValued c a
  QuotT (CValued c a) (SValued c a) = CValued c a
  QuotT (CValued c a) (CValued c a) = SValued c a
  QuotT (CValued c a) a             = CValued c a
  QuotT (SValued c a) a             = SValued c a
  QuotT a             (SValued c a) = SValued c a


class (c ~ QuotT a b)=>HasDivision a b c where
    (|/|) :: a -> b -> c

instance (RealFrac a, q ~ SValued c a, QuotT q q ~ q)=>HasDivision (SValued c a) (SValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasDivision (SValued Currency Double) (SValued Currency Double) (SValued Currency Double) #-}
  a |/| b = scBinOp (/) a b
  {-# INLINABLE (|/|) #-}

instance (RealFrac a, q ~ CValued c a, QuotT q (SValued c a) ~ q) => HasDivision (CValued c a) (SValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasDivision (CValued Currency Double) (SValued Currency Double) (CValued Currency Double) #-}
  (CVCV g) |/| (CVS x) = CVCV (\c e -> g c e/x)
  (CVCV g) |/| (CVEV f) = CVCV (\c e -> g c e/f e)
  (CVMV x c h) |/| (CVS y) = CVMV (x/y) c h
  (CVMV x c h) |/| (CVEV f) = CVCV (\c' e -> (h e c' c x)/f e)
  {-# INLINABLE (|/|) #-}

instance (Eq c, HasOneValue c,RealFrac a, q ~ CValued c a, QuotT q q ~ SValued c a)=>HasDivision (CValued c a) (CValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasDivision (CValued Currency Double) (CValued Currency Double) (SValued Currency Double) #-}
  a |/| b = ciBinOp (/) a b
  {-# INLINABLE (|/|) #-}

instance (RealFrac a, q ~ SValued c a, QuotT q a ~ q) => HasDivision (SValued c a) a (SValued c a) where
  {-# SPECIALIZE instance HasDivision (SValued Currency Double) Double (SValued Currency Double) #-}
  (CVS x)  |/| y = CVS (x/y)
  (CVEV f) |/| y = CVEV $ \e -> (f e)/y
  {-# INLINABLE (|/|) #-}

instance (RealFrac a, q ~ CValued c a, QuotT q a ~ q) => HasDivision (CValued c a) a (CValued c a) where
  {-# SPECIALIZE instance HasDivision (CValued Currency Double) Double (CValued Currency Double) #-}
  (CVCV g) |/| y = CVCV (\c' e -> (g c' e)/y)
  (CVMV x c h) |/| y = CVMV (x/y) c h
  {-# INLINABLE (|/|) #-}

instance (RealFrac a, q ~ SValued c a, QuotT a q ~ q) => HasDivision a (SValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasDivision Double (SValued Currency Double) (SValued Currency Double) #-}
  x |/| (CVS y) = CVS (x/y)
  y |/| (CVEV f) = CVEV $ \e -> y/f e
  {-# INLINABLE (|/|) #-}

class HasEqOrdering a where
  type ReturnType a :: *
  (|>|),(|>=|),(|<|),(|<=|),(|==|),(|/=|)::a->a->ReturnType a

class HasMinMax a where
  cvMin,cvMax::a->a->a


instance (Eq a, Ord a)=>HasEqOrdering (SValued c a) where
  {-# SPECIALIZE instance HasEqOrdering (SValued Currency Double) #-}
  type ReturnType (SValued c a) = SValued c Bool
  (|>|)  = scBinOp (>)
  {-# INLINABLE (|>|) #-}                                                                            
  (|>=|) = scBinOp (>=)
  {-# INLINABLE (|>=|) #-}                                                                            
  (|<|)  = scBinOp (<)
  {-# INLINABLE (|<|) #-}                                                                            
  (|<=|) = scBinOp (<=)
  {-# INLINABLE (|<=|) #-}                                                                            
  (|==|) = scBinOp (==)
  {-# INLINABLE (|==|) #-}                                                                            
  (|/=|) = scBinOp (/=)
  {-# INLINABLE (|/=|) #-}

instance Ord a=>HasMinMax (SValued c a) where
  {-# SPECIALIZE instance HasMinMax (SValued Currency Double) #-}
  cvMin = scBinOp min
  {-# INLINABLE cvMin #-}                                                                            
  cvMax = scBinOp max
  {-# INLINABLE cvMax #-}

instance (Eq c,Eq a, Ord a,HasOneValue c)=>HasEqOrdering (CValued c a) where
  {-# SPECIALIZE instance HasEqOrdering (CValued Currency Double) #-}
  type ReturnType (CValued c a) = SValued c Bool
  (|>|)  = ciBinOp (>)
  {-# INLINABLE (|>|) #-}                                                                            
  (|>=|) = ciBinOp (>=)
  {-# INLINABLE (|>=|) #-}                                                                            
  (|<|)  = ciBinOp (<)
  {-# INLINABLE (|<|) #-}                                                                            
  (|<=|) = ciBinOp (<=)
  {-# INLINABLE (|<=|) #-}                                                                            
  (|==|) = ciBinOp (==)
  {-# INLINABLE (|==|) #-}                                                                            
  (|/=|) = ciBinOp (/=)
  {-# INLINABLE (|/=|) #-}

-- In preliminary testing this is slower!!
cvOrdOp::Eq c=>(a->a->a)->CValued c a->CValued c a->CValued c a
cvOrdOp ordOp a@(CVMV x cx hx) b@(CVMV y cy hy)
  | cx == cy = CVMV (ordOp x y) cx hx
  | otherwise = cvBinOp ordOp a b
cvOrdOp ordOp a b = cvBinOp ordOp a b
{-# INLINABLE cvOrdOp #-}

instance (Eq c, Ord a)=>HasMinMax (CValued c a) where
  {-# SPECIALIZE instance HasMinMax (CValued Currency Double) #-}
  cvMin = cvOrdOp min
  {-# INLINABLE cvMin #-}                                                                            
  cvMax = cvOrdOp max
  {-# INLINABLE cvMax #-}                                                                            


scCompare::Ord a=>SValued c a->SValued c a->SValued c Ordering
scCompare = scBinOp compare
{-# INLINABLE scCompare #-}

cvCompare::Ord a=>CValued c a->CValued c a->CValued c Ordering
cvCompare = cvBinOp compare
{-# INLINABLE cvCompare #-}


type family IfT (a :: *) (b :: *) :: * where
  IfT (CValued c Bool) (SValued c a) = CValued c a
  IfT (CValued c Bool) (CValued c a) = CValued c a
  IfT (SValued c Bool) (SValued c a) = SValued c a
  IfT (SValued c Bool) (CValued c a) = CValued c a
  IfT (CValued c Bool) a             = CValued c a
  IfT (SValued c Bool) a             = SValued c a

class (r ~ IfT a b) => HasIf a b r where
  cvIf::a->b->b->r

instance (q x ~ SValued c x, IfT (q Bool) (q a) ~ (q a)) => HasIf (SValued c Bool) (SValued c a) (SValued c a) where
  {-# SPECIALIZE instance HasIf (SValued Currency Bool) (SValued Currency Double) (SValued Currency Double) #-}
  cvIf (CVS x) a b = if x then a else b
  cvIf (CVEV fx) a b = CVEV $ \e -> if fx e then atERF e a else atERF e b
  {-# INLINABLE cvIf #-}                                                                            

instance (Eq c, q x ~ CValued c x, IfT (q Bool) (q a) ~ q a)=>HasIf (CValued c Bool) (CValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasIf (CValued Currency Bool) (CValued Currency Double) (CValued Currency Double) #-}
  cvIf (CVCV gx) a b = CVCV $ \c e -> if gx c e then unwrap c e a else unwrap c e b
  cvIf (CVMV x cx hx) a b = CVCV $ \c e -> if hx e c cx x then unwrap c e a else unwrap c e b
  {-# INLINABLE cvIf #-}                                                                            

instance (Eq c, q ~ CValued c a, IfT (SValued c Bool) q ~ q)=>HasIf (SValued c Bool) (CValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasIf (SValued Currency Bool) (CValued Currency Double) (CValued Currency Double) #-}
  cvIf (CVS x) a b = if x then a else b
  cvIf (CVEV fx) a b = CVCV $ \c e -> if fx e then unwrap c e a else unwrap c e b
  {-# INLINABLE cvIf #-}                                                                                                                                   

instance (q x ~ CValued c x, IfT (q Bool) (SValued c a) ~ q a) => HasIf (CValued c Bool) (SValued c a) (CValued c a) where
  {-# SPECIALIZE instance HasIf (CValued Currency Bool) (SValued Currency Double) (CValued Currency Double) #-}
  cvIf (CVCV gx) a b = CVCV $ \c e -> if gx c e then atERF e a else atERF e b
  cvIf (CVMV x cx hx) a b = CVCV $ \c e -> if hx e c cx x then atERF e a else atERF e b
  {-# INLINABLE cvIf #-}                                                                                                                                   

instance (q x ~ SValued c x, IfT (q Bool) a ~ q a) => HasIf (SValued c Bool) a (SValued c a) where
  {-# SPECIALIZE instance HasIf (SValued Currency Bool) Double (SValued Currency Double) #-}
  cvIf (CVS cond) a b = if cond then CVS a else CVS b
  cvIf (CVEV f) a b = CVEV $ \e -> if f e then a else b
  {-# INLINABLE cvIf #-}                                                                                                                                   

instance (q x ~ CValued c x, IfT (q Bool) a  ~ q a) => HasIf (CValued c Bool) a (CValued c a) where
  {-# SPECIALIZE instance HasIf (CValued Currency Bool) Double (CValued Currency Double) #-}
  cvIf (CVCV g) a b = CVCV (\c' e -> if g c' e then a else b)
  cvIf (CVMV x c h) a b = CVCV (\c' e -> if h e c' c x then a else b)
  {-# INLINABLE cvIf #-}                                                                                                                                   

{-
scChoose::SValued c Bool->a->a->SValued c a
scChoose (CVS cond) a b = if cond then CVS a else CVS b 
scChoose (CVEV f) a b = CVEV $ \e -> if (f e) then a else b
{-# INLINABLE scChoose #-}

cvChoose::CValued c Bool->a->a->CValued c a
cvChoose (CVCV g) a b = CVCV (\c' e -> if (g c' e) then a else b)
cvChoose (CVMV x c h) a b = CVCV (\c' e -> if (h e c' c x) then a else b)
{-# INLINABLE cvChoose #-}
-}

class HasCase a b where
  cvCase::[(a,b)]->b->b

instance HasCase (SValued c Bool) (SValued c a) where
  {-# SPECIALIZE instance HasCase (SValued Currency Bool) (SValued Currency Double) #-}
  cvCase cases dflt = CVEV $ \e -> atERF e . snd $ foldl (f e) (False,dflt) cases where
    f::ERF c->(Bool,a)->(SValued c Bool,a)->(Bool,a)
    f e (foundYet,currentAction) (cond,action)
      | foundYet       = (True,currentAction)
      | atERF e cond   = (True,action)
      | otherwise      = (False,currentAction)
  {-# INLINABLE cvCase #-}

instance Eq c=>HasCase (SValued c Bool) (CValued c a) where
  {-# SPECIALIZE instance HasCase (SValued Currency Bool) (CValued Currency Double) #-}
  cvCase cases dflt = CVCV $ \c e -> unwrap c e . snd $ foldl (f e) (False,dflt) cases where
    f::ERF c->(Bool,a)->(SValued c Bool,a)->(Bool,a)
    f e (foundYet,currentAction) (cond,action)
      | foundYet       = (True,currentAction)
      | atERF e cond   = (True,action)
      | otherwise      = (False,currentAction)
  {-# INLINABLE cvCase #-}


class HasAndOr a where
  cvAnd::a->a->a
  cvOr::a->a->a


instance HasAndOr (SValued c Bool) where
  {-# SPECIALIZE instance HasAndOr (SValued Currency Bool) #-}
  cvAnd = scBinOp (&&)
  {-# INLINABLE cvAnd #-}                                                                            
  cvOr = scBinOp (||)
  {-# INLINABLE cvOr #-}                                                                            


instance HasAndOr (CValued c Bool) where
  {-# SPECIALIZE instance HasAndOr (CValued Currency Bool) #-}
  cvAnd = cvBinOp (&&)
  {-# INLINABLE cvAnd #-}                                                                            
  cvOr = cvBinOp (||)
  {-# INLINABLE cvOr #-}                                                                            

