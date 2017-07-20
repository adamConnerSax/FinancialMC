{-# LANGUAGE GADTs #-}
module Probability
  (
  ) where

import Data.Random as R
import Data.Random.RVar as R
import Data.Random.Distribution.Normal (Normal (..), normal)
import Data.Random.Distribution.Uniform (Uniform (..), uniform)
import Data.Random.Distribution.Gamma (Gamma (..), normal)
import Data.Random.Source.PureMT as MT

import Control.Monad.State (evalState)
import Control.Arrow (first)

newtype Prob = Prob { toDouble :: Double } deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Floating, Random)

newtype Explicit a = Explicit { toList :: [(a, Prob)] }

instance Functor Explicit where
  fmap h = Explicit . fmap (first h) . toList 

instance Applicative Explicit where
  pure x = Explicit [(x, 1)]
  (Explicit fs) <*> (Explicit as) = Explicit $ [(f a, pf * pa) | (f, pf) <- fs, (a, pa) <- as]

instance Monad Explicit where
  return = pure
  (Explicit as) >>= f = Explicit [ (a, pa * pb) | (a, pa) <- as, (b, pb) <- toList (f a)] 


class Sampleable d where
  sample :: RandomGen g => g -> d a -> a

data Dist a where
  Return :: a -> Dist a
  Bind :: Dist b -> (b -> Dist a) -> Dist a
  Primitive :: Sampleable d => d a -> Dist a
  Conditional :: (a -> Prob) -> Dist a -> Dist a
  
condition :: (a -> prob) -> Dist a -> Dist a
condition = Conditional

instance Functor Dist where
  fmap h da = Bind da (Return . h) -- liftM

instance Functor Dist => Applicative Dist where
  pure = Return
  (<*>) dfab da = Bind (fmap (\h -> fmap h da) dfab) id -- ap ?
  
instance Applicative Dist => Monad Dist where
  return = Return
  (>>=) = Bind

instance Sampleable Dist where
  sample g (Return x) = x
  sample g (Bind da f) = sample g1 y where
    y = f (sample g2 d)
    (g1, g2) = split g
  sample g (Primitive da) = sample g da
  sample g (Conditional f da) = undefined
  
sampleState = runState . R.sampleRVar

-- NB these Sampleable instances all throw away the new generator.  What's up with that?

instance Sampleable [a] where
  sample g as = fst $ sampleState (R.randomElement as) g 

instance R.Distribution Uniform a => Sampleable (Uniform a) where
  sample g (Uniform x y) = fst $ sampleState (uniform x y)

instance R.Distribution Normal a => Sampleable (Normal a) where
  sample g (Normal m sd) = fst $ sampleState (normal m sd) g

instance R.Distribution Gamma a => Sampleable (Gamma a) where
  sample g (Gamma x y) = fst $ sampleState (gamma x y) g

uniformList :: [a] -> Dist a
uniformList = Primitive

uniformLoHi :: R.Distribution Uniform a => a -> a -> Dist a
uniformLoHi lo hi = Primitive (Uniform lo hi) 

normal :: (Real a, R.Distribution Normal a) => a -> a -> Dist a
normal mean sd = Primitive (Normal mean sd) 

gamma :: (Floating a, Ord a, Distribution Normal a, Distribution StdUniform a) => a -> a -> Dist a
gamma x y = Primitive (Gamma x y)


