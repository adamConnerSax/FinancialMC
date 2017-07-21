{-# LANGUAGE GADTs #-}
module FinancialMC.Core.Probability
  (
  ) where

import Data.Random as R
import Data.Random.RVar as R
import Data.Random.Distribution.Normal (Normal (Normal), normal)
import Data.Random.Distribution.Uniform (Uniform (Uniform), uniform)
import Data.Random.Distribution.Gamma (Gamma (Gamma), gamma)
import Data.Random.Distribution.Bernoulli (Bernoulli (Bernoulli), bernoulli)
import Data.Random.Distribution.Categorical (Categorical, categorical, fromList, fromWeightedList)
import Data.Random.Sample (Sampleable (sampleFrom), sample, sampleState, sampleStateT)
import Data.Random.Source.PureMT as MT
import Data.Random.Source (RandomSource)

import Control.Monad.State (evalState)
import Control.Arrow (first)

newtype Prob = Prob { toDouble :: Double } deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Floating {-, Random -})

{-  
class Sampleable d where
  sample :: RandomGen g => g -> d a -> m a
-}
  
data Dist m a where
  Return :: a -> Dist m a
  Bind :: Dist m b -> (b -> Dist m a) -> Dist m a
  Primitive :: Sampleable d m a => d a -> Dist m a
  Conditional :: (a -> Prob) -> Dist m a -> Dist m a
  
condition :: (a -> Prob) -> Dist m a -> Dist m a
condition = Conditional

instance Functor (Dist m) where
  fmap h da = Bind da (Return . h) -- liftM

instance Functor (Dist m) => Applicative (Dist m) where
  pure = Return
  (<*>) dfab da = Bind (fmap (\h -> fmap h da) dfab) id -- ap ?
  
instance Applicative (Dist m) => Monad (Dist m) where
  return = Return
  (>>=) = Bind

-- Sampleable from random-fu
-- sampleFrom :: RandomSource m s => s -> d a -> m a
instance RandomSource m s => Sampleable (Dist m) m a where
  sampleFrom s (Return x) = return x
  sampleFrom s (Bind da f) = do
    a <- sampleFrom s da
    sampleFrom s (f a)
  sampleFrom s (Primitive da) = sampleFrom s da

{-
-- Sampleable from monad-bayes
instance Sampleable Dist where
  sample g (Return x) = x
  sample g (Bind da f) = sampleFrom g1 y where
    y = f (sample g2 da)
    (g1, g2) = split g
  sample g (Primitive da) = sample g da
  sample g (Conditional f da) = undefined
-}


--sampleState = runState . R.sampleRVar

-- NB these Sampleable instances all throw away the new generator.  What's up with that?
{-
instance Sampleable [a] where
  sample g as = fst $ sampleState (R.randomElement as) g 

instance R.Distribution Uniform a => Sampleable (Uniform a) where
  sample g (Uniform x y) = fst $ sampleState (uniform x y)

instance R.Distribution Normal a => Sampleable (Normal a) where
  sample g (Normal m sd) = fst $ sampleState (normal m sd) g

instance R.Distribution Gamma a => Sampleable (Gamma a) where
  sample g (Gamma x y) = fst $ sampleState (gamma x y) g

instance R.Distribution Bernoulli a => Sampleable (Bernoulli a) where
  sample g (Bernoulli b) = fst $ sampleState (bernoulli b)
-}

uniformList :: [a] -> Dist a
uniformList = Primitive

uniformLoHi :: R.Distribution Uniform a => a -> a -> Dist a
uniformLoHi lo hi = Primitive (Uniform lo hi) 

normal :: (Real a, R.Distribution Normal a) => a -> a -> Dist a
normal mean sd = Primitive (Normal mean sd) 

gamma :: (Floating a, Ord a, Distribution Normal a, Distribution StdUniform a) => a -> a -> Dist a
gamma x y = Primitive (Gamma x y)

bernoulli :: R.Distribution Bernoulli a => a -> Dist a
bernoulli x = Primitive (Bernoulli x)

categorical :: R.Distribution (Categorical Prob a) => [(Prob, a)] -> Dist a
categorical probAs = Primitive (fromList probAs)

categorical' :: R.Distribution (Categorical Prob a) => [(Prob, a)] -> Dist a
categorical' weightedAs = Primitive (fromWeightedList weightedAs)


