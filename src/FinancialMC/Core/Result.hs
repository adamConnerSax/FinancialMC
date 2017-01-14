{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,UndecidableInstances #-}
module FinancialMC.Core.Result 
       (
         Result(..),
         MonadResult(returnOnly,appendAndReturn,adjust),
         ResultT(..),runResultT
       ) where


import Control.Monad.Trans (MonadTrans,lift)
import Control.Monad.Morph (MFunctor(..))
import Control.Monad.Reader (MonadReader(..),local)
import Control.Monad.State (MonadState,get,put)
import Control.Monad.Catch (MonadThrow(..))

-- This is isomorphic to writer; Result o a is isomorphic to (o,a)
-- Redone as Continuation passing form of stateT
    
data Result o a = Result a o 

instance Functor (Result o) where 
  fmap f (Result a o) = Result (f a) o
  
instance Monoid o=>Applicative (Result o) where
  pure a = Result a mempty
  (Result f oF) <*> (Result a oA) = 
    let ot = oF `mappend` oA 
     in ot `seq` Result (f a) (oF `mappend` oA) where
      
      
instance Monoid o=>Monad (Result o) where
  return = pure
  (Result a o) >>= f = 
    let Result b o' = f a
        ot = o `mappend` o'
     in ot `seq` Result b (o `mappend` o') 
        
-- NB: fundep below (m->o) required for returnOnly to work since nothing of type o is an arg.  This is confusing.
class (Monoid o,Monad m)=>MonadResult o m | m->o where
  returnOnly::a->m a
  appendAndReturn::o->a->m a
  adjust::(o->m o)->m a->m a

instance Monoid o=>MonadResult o (Result o) where
  returnOnly a = Result a mempty
  appendAndReturn o a = Result a o
  adjust f (Result a o) = do
    o' <- f o
    Result a o'
  
newtype ResultT o m a = ResultT { unResultT::o->m (Result o a) }

runResultT::Monoid o=>ResultT o m a->m (Result o a)
runResultT m = unResultT m mempty

instance Monad m=>Functor (ResultT o m) where
  fmap f r = ResultT $ \o->do
    result <- unResultT r o
    return $! fmap f result

instance (Monad m, Monoid o)=>Applicative (ResultT o m) where
  pure x = ResultT $ \o->return (Result x o)
  rf <*> ra = ResultT $ \o -> do
    Result f o' <- unResultT rf o
    Result a o'' <- unResultT ra o'
    return $! Result (f a)  o'' 
    
instance (Monad m, Monoid o)=>Monad (ResultT o m) where
  return x = ResultT $ \o->return (Result x o)

  ma >>= f = ResultT $ \o-> do 
    Result a oA <- unResultT ma o
    unResultT (f a) oA    
    
instance (Monad m,Monoid o)=>MonadResult o (ResultT o m) where  
  returnOnly = return 
  appendAndReturn o a = ResultT $ \x->
    let ox = o `mappend` x 
     in ox `seq` unResultT (return a) (o `mappend` x)
    
  adjust f rt = ResultT $ \x->do
    Result a o <- unResultT rt x
    Result o' _ <- unResultT (f o) o 
    return $! Result a o'
    
    
instance Monoid o=>MonadTrans (ResultT o) where
  lift m = ResultT $ \o-> do
    a <- m
    return $! Result a o
  
instance Monoid o=>MFunctor (ResultT o) where
    hoist m2n rtm = ResultT (m2n . unResultT rtm)

instance (Monoid o,MonadReader r m)=>MonadReader r (ResultT o m) where
  ask = lift ask
  local f x = ResultT (local f . unResultT x) 
            
instance (Monoid o, MonadState s m)=>MonadState s (ResultT o m) where
  get = lift get
  put k = lift $ put k

instance (Monoid o,MonadThrow m)=>MonadThrow (ResultT o m) where
  throwM = lift . throwM
