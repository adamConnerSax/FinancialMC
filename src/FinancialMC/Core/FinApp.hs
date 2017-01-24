{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-} 
module FinancialMC.Core.FinApp 
       (
         LogLevel(..)
       , faLog
       , toStepApp
       , toPathApp
       , taxDataApp2StepAppFS
       , taxDataApp2StepAppFSER
       , taxDataApp2StepApp
       , zoomStepApp
       , magnifyStepApp
       , zoomPathAppS
       , zoomPathAppE
       , LoggableStepApp(..)
       , LoggablePathApp(..)
       , StepApp
       , execPathApp
       , execPPathApp
       ) where

import           FinancialMC.Core.MoneyValue (ExchangeRateFunction)
import           FinancialMC.Core.FinancialStates (FinEnv,HasFinEnv(..),FinState,HasFinState(..))
import           FinancialMC.Core.Tax (TaxDataApp, TaxData)
import           FinancialMC.Core.Utilities (eitherToIO,multS,readOnly)

import           Control.Lens (makeClassy,Lens',zoom,magnify,Identity,(^.),set)

import           Control.Monad (when,unless)
import           Control.Monad.Catch (MonadThrow(..),SomeException)
import           Control.Monad.Morph (generalize,hoist)
import           Control.Monad.Reader (ReaderT,MonadReader)
import           Control.Monad.State.Strict (StateT,MonadState,execStateT)
import           Control.Monad.Trans (lift,MonadIO,liftIO,MonadTrans)

import           Pipes (MFunctor,runEffect,yield,await,Producer,Proxy,X,(>->))


data LogLevel = Debug | Info   deriving (Enum,Show,Eq,Bounded)
data LogEntry = LogEntry { _leLevel::LogLevel, _leMsg::String }
makeClassy ''LogEntry


{-
newtype ROBaseStepApp s e m a =
  ROBaseStepApp { unROBaseStepApp::ReaderT s (ReaderT e m) a }
  deriving (Functor,Applicative,Monad)

instance MFunctor (ROBaseStepApp s e) where
  hoist f (ROBaseStepApp rbsa) = ROBaseStepApp $ hoist (hoist f) rbsa

instance MonadTrans (ROBaseStepApp s e) where
  lift m = ROBaseStepApp . lift $ lift m

instance MonadThrow m=>MonadThrow (ROBaseStepApp s e m) where
  throwM x = lift $ throwM x
-}

newtype BaseStepApp s e m a = 
  BaseStepApp { unBaseStepApp::StateT s (ReaderT e m) a } 
  deriving (Functor,Applicative,Monad,MonadState s,MonadReader e)

instance MFunctor (BaseStepApp s e) where
  hoist f (BaseStepApp bsa) = BaseStepApp $ hoist (hoist f) bsa 

instance MonadTrans (BaseStepApp s e) where
  lift m = BaseStepApp . lift $ lift m

instance MonadThrow m=>MonadThrow (BaseStepApp s e m) where
  throwM x = lift $ throwM x
          
newtype StepApp s e a = 
  StepApp { unStepApp::BaseStepApp s e (Either SomeException) a } deriving (Functor,Applicative,Monad,MonadState s,MonadReader e)

instance MonadThrow (StepApp s e) where
  throwM x = StepApp $ throwM x

{-
newtype ROStepApp s e a =
  ROStepApp { unROStepApp::ROBaseStepApp s e (Either SomeException) a } deriving (Functor,Applicative,Monad)

instance MonadThrow (ROStepApp s e) where
  throwM x = ROStepApp $ throwM x
-}

newtype PStepApp s e a = 
  PStepApp { unPStepApp::Producer LogEntry (BaseStepApp s e IO) a } deriving (Functor,Applicative,Monad,MonadState s, MonadReader e)

instance MonadThrow (PStepApp s e) where
  throwM x = PStepApp . lift . lift $ throwM x
  
instance (MonadIO (BaseStepApp s e IO))=>MonadIO (PStepApp s e) where
  liftIO m = PStepApp $ liftIO m


{-
newtype ROPStepApp s e a = 
  ROPStepApp { unROPStepApp::Producer LogEntry (ROBaseStepApp s e IO) a } deriving (Functor,Applicative,Monad)

instance MonadThrow (ROPStepApp s e) where
  throwM x = ROPStepApp . lift . lift $ throwM x
  
instance (MonadIO (ROBaseStepApp s e IO))=>MonadIO (ROPStepApp s e) where
  liftIO m = ROPStepApp $ liftIO m

--readOnlyStepApp::ROStepApp s e a->StepApp s e a
--readOnlyStepApp (ROStepApp rbsa) = let (ROBaseStepApp stack) = rbsa in StepApp . BaseStepApp $  readOnly stack
-}

zoomBaseStepApp::Monad m=>Lens' s2 s1->BaseStepApp s1 e m a->BaseStepApp s2 e m a
zoomBaseStepApp l (BaseStepApp bsa) = BaseStepApp $ zoom l bsa

magnifyBaseStepApp::Monad m=>Lens' e2 e1->BaseStepApp s e1 m a->BaseStepApp s e2 m a
magnifyBaseStepApp l (BaseStepApp bsa) = BaseStepApp $ hoist (magnify l) bsa

zoomStepApp::Lens' s2 s1->StepApp s1 e a->StepApp s2 e a
zoomStepApp l (StepApp sa) = StepApp $ zoomBaseStepApp l sa

magnifyStepApp::Lens' e2 e1->StepApp s e1 a->StepApp s e2 a
magnifyStepApp l (StepApp sa) = StepApp $ magnifyBaseStepApp l sa 

toStepApp::StateT s (ReaderT e (Either SomeException)) a->StepApp s e a
toStepApp x = StepApp $ BaseStepApp x


newtype BasePathApp s e m a = 
  BasePathApp { unBasePathApp::StateT (s,e) m a } 
  deriving (Functor,Applicative,Monad,MonadState (s,e),MFunctor,MonadTrans)

newtype PathApp s e a = 
  PathApp { unPathApp::BasePathApp s e (Either SomeException) a} deriving (Functor,Applicative,Monad,MonadState (s,e))

instance MonadThrow (PathApp s e) where
  throwM x = PathApp . lift $ throwM x

newtype PPathApp s e a = 
  PPathApp { unPPathApp::Producer LogEntry (BasePathApp s e IO) a} deriving (Functor,Applicative,Monad,MonadState (s,e))
                                                                     
instance MonadThrow (PPathApp s e) where
  throwM x = PPathApp . lift . lift $ throwM x
  
instance MonadIO (BasePathApp s e IO) where 
  liftIO m = BasePathApp $ liftIO m

instance (MonadIO (BasePathApp s e IO))=>MonadIO (PPathApp s e) where
  liftIO m = PPathApp $ liftIO m




lensFirst::Lens' a b->Lens' (a,x) (b,x)
lensFirst l q (a,x) = (\(b,y) -> (set l b a,y)) <$> q (a ^. l,x)

lensSecond::Lens' a b->Lens' (x,a) (x,b)
lensSecond l q (x,a) = (\(y,b)->(y,set l b a)) <$> q (x, a ^. l)


zoomBasePathAppS::Monad m=>Lens' s2 s1->BasePathApp s1 e m a->BasePathApp s2 e m a
zoomBasePathAppS l (BasePathApp bpa) = BasePathApp $ zoom (lensFirst l) bpa

zoomPathAppS::Lens' s2 s1->PathApp s1 e a->PathApp s2 e a
zoomPathAppS l (PathApp pa) = PathApp $ zoomBasePathAppS l pa

zoomBasePathAppE::Monad m=>Lens' e2 e1->BasePathApp s e1 m a->BasePathApp s e2 m a
zoomBasePathAppE l (BasePathApp bpa) = BasePathApp $ zoom (lensSecond l) bpa

zoomPathAppE::Lens' e2 e1->PathApp s e1 a->PathApp s e2 a
zoomPathAppE l (PathApp pa) = PathApp $ zoomBasePathAppE l pa


toPathApp::StateT (s,e) (Either SomeException) a->PathApp s e a
toPathApp x = PathApp $ BasePathApp x


execBasePathApp::Monad m=>BasePathApp s e m a->s->e->m (s,e)
execBasePathApp bpa st env = execStateT (unBasePathApp bpa) (st,env)

execPathApp::PathApp s e a->s->e->Either SomeException (s,e)                 
execPathApp app = execBasePathApp (unPathApp app)

execPPathApp::PPathApp s e a->[LogLevel]->s->e->IO (s,e)                 
execPPathApp app logDetails  = execBasePathApp (runEffect (unPPathApp app >-> printLog logDetails))

liftStepApp::StepApp s e a->PStepApp s e a
liftStepApp (StepApp x) = PStepApp . lift $ hoist eitherToIO x -- adds IO at bottom of stack and wraps in Producer 

liftPathApp::PathApp s e a->PPathApp s e a
liftPathApp (PathApp x) = PPathApp . lift $ hoist eitherToIO x

baseStep2basePath::Monad m=>BaseStepApp s e m a->BasePathApp s e m a
baseStep2basePath (BaseStepApp bsa) = BasePathApp . multS $ hoist readOnly bsa


class (MonadThrow a, MonadState s a, MonadReader e a)=>LoggableStepApp s e a where
  stepLog::LogLevel->String->a ()
  stepLift::StepApp s e z->a z
  fromBaseS::BaseStepApp s e Identity x->a x
  zoomStep::Lens' s sInner->StepApp sInner e x->a x
  zoomStep l sa = stepLift $ zoomStepApp l sa

instance LoggableStepApp s e (StepApp s e) where
  stepLog _ _ = return ()
  stepLift = id
  fromBaseS x = StepApp $ hoist generalize x
  
instance LoggableStepApp s e (PStepApp s e) where
  stepLog ll msg = PStepApp $ faLog ll msg
  stepLift = liftStepApp
  fromBaseS x = PStepApp . lift $ hoist generalize x


class (MonadThrow a, MonadState (s,e) a)=>LoggablePathApp s e a where
  type Step s e a:: * -> *
  pathLog::LogLevel->String->a ()
  pathLift::PathApp s e z->a z
  fromBaseP::BasePathApp s e Identity x->a x
  step2Path::Step s e a x->a x 

instance LoggablePathApp s e (PathApp s e) where
  type Step s e (PathApp s e)  = StepApp s e
  pathLog _ _ = return ()
  pathLift = id
  fromBaseP x = PathApp $ hoist generalize x
  step2Path (StepApp sa) = PathApp $ baseStep2basePath sa
  
instance LoggablePathApp s e (PPathApp s e) where
  type Step s e (PPathApp s e) = PStepApp s e
  pathLog ll msg = PPathApp $ faLog ll msg
  pathLift = liftPathApp
  fromBaseP x = PPathApp . lift $ hoist generalize x
  step2Path (PStepApp psa) = PPathApp $ hoist baseStep2basePath psa


faLog::Monad m=>LogLevel->String->Producer LogEntry m ()
faLog l msg  = yield $ LogEntry l msg

printLog::(MonadIO m) => [LogLevel]->Proxy () LogEntry () X  m a
printLog lvls = do
  let printL x = do 
        let lvl = x ^. leLevel 
            msg = x ^. leMsg
            shouldLog = lvl `elem` lvls
        when shouldLog (liftIO $ putStrLn (show lvl ++ ": " ++ msg))
  le <- await
  unless (null lvls) (printL le)  
  printLog lvls   


taxDataApp2StepAppFSER::LoggableStepApp FinState ExchangeRateFunction app => TaxDataApp (Either SomeException) a->app a          
taxDataApp2StepAppFSER tda = stepLift . (zoomStepApp fsTaxData).  toStepApp $ tda


taxDataApp2StepAppFS::TaxDataApp (Either SomeException) a->StepApp FinState (FinEnv rm) a          
taxDataApp2StepAppFS tda = (zoomStepApp fsTaxData) . (magnifyStepApp feExchange) . toStepApp $ tda

taxDataApp2StepApp::LoggableStepApp TaxData ExchangeRateFunction app=>TaxDataApp (Either SomeException) a->app a          
taxDataApp2StepApp tda = stepLift . toStepApp $ tda

{-
taxDataApp2StepAppFS'::(LoggableStepApp FinState FinEnv app, TaxDataAppC m)=>m a->app a
taxDataApp2StepAppFS' tda = zoomStepApp fsTaxData . magnifyStepApp feExchange 


f::(Functor (Zoomed m a),Zoom m n ExchangeRateFunction FinEnv)=>m a->n a
f = zoom feExchange

g::(Functor (Magnified m a),Magnify m n TaxData FinState)=>m a->n a
g = magnify fsTaxData

-}
