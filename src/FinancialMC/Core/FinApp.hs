{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module FinancialMC.Core.FinApp
       (
         LogLevel(..)
       , PathState (..)
       , pattern PathState
       , stepState
       , stepEnv
       , zoomPathState
       , ReadOnly (..)
       , Loggable (log)
       , execPathStack
       , execPPathStack
-- optimization
       , PathStackable (..)
-- only for Bench/Profiling
       , BasePathStack (..)
       , PathStack (..)
       , PPathStack (..)
       ) where

import           Prelude                          hiding (log)

import           FinancialMC.Core.FinancialStates (FinEnv, FinState,
                                                   HasFinEnv (..),
                                                   HasFinState (..))
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction)
import           FinancialMC.Core.Result          (ResultT (..))
import           FinancialMC.Core.Utilities       (AsFMCException, FMCException,
                                                   HasFMCException, eitherToIO,
                                                   multS, readOnly)

import           Control.Lens                     (Getter, Identity, Lens',
                                                   lens, magnify, makeClassy,
                                                   set, view, zoom, (^.), _1,
                                                   _2)

import           Control.Lens.Zoom                (Zoom (..), Zoomed)

import           Control.Monad                    (unless, when)
import qualified Control.Monad.Base               as MB
import           Control.Monad.Catch              (MonadThrow (..),
                                                   SomeException, catch, throwM,
                                                   toException)
import           Control.Monad.Except             (MonadError (..))
import           Control.Monad.Morph              (generalize, hoist)
import           Control.Monad.Reader             (MonadReader,
                                                   ReaderT (ReaderT), ask,
                                                   runReaderT)
import           Control.Monad.State.Strict       (MonadState, StateT (StateT),
                                                   execStateT, get, put,
                                                   runStateT)
import           Control.Monad.Trans              (MonadIO, MonadTrans, lift,
                                                   liftIO)
import qualified Control.Monad.Trans.Control      as MTC
import           Data.Text                        (Text, unpack)

import           Pipes                            (MFunctor, Producer, Proxy, X,
                                                   await, runEffect, yield,
                                                   (>->))


data LogLevel = Debug | Info   deriving (Enum, Show, Eq, Bounded)
data LogEntry = LogEntry { _leLevel :: LogLevel, _leMsg :: Text }
makeClassy ''LogEntry


data PathState s e = MkPathState { _stepState :: !s, _stepEnv :: !e } deriving (Show)
makeClassy ''PathState

{-
type PathState s e = (s, e)

stepState :: Lens' (PathState s e) s
stepState = _1

stepEnv :: Lens' (PathState s e) e
stepEnv = _2
-}

pattern PathState s e = MkPathState s e

zoomPathState :: Lens' s1 s2 -> Lens' e1 e2 -> Lens' (PathState s1 e1) (PathState s2 e2)
zoomPathState ls le = lens (\(PathState s e) -> PathState (view ls s) (view le e)) (\(PathState s1 e1) (PathState s2 e2) -> PathState (set ls s2 s1) (set le e2 e1))

newtype BasePathStack s m a =
  BasePathStack { unBasePathStack :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MFunctor, MonadIO, MonadState s)

deriving instance MB.MonadBase b m => MB.MonadBase b (BasePathStack s m)

instance MTC.MonadTransControl (BasePathStack s) where
  type StT (BasePathStack s) a = MTC.StT (StateT s) a
  liftWith = MTC.defaultLiftWith BasePathStack unBasePathStack
  restoreT = MTC.defaultRestoreT BasePathStack

instance MTC.MonadBaseControl b m => MTC.MonadBaseControl b (BasePathStack s m) where
  type StM (BasePathStack s m) a = MTC.ComposeSt (BasePathStack s) m a
  liftBaseWith = MTC.defaultLiftBaseWith
  restoreM = MTC.defaultRestoreM

instance (MTC.MonadBaseControl m (BasePathStack s m), MonadError err m) => MonadError err (BasePathStack s m) where
  throwError = lift . throwError
  catchError action handler = MTC.control $ \run -> catchError (run action) (run . handler)

instance MonadThrow m => MonadThrow (BasePathStack s m) where
  throwM = lift . throwM

type instance Zoomed (BasePathStack s m)  = Zoomed (StateT s m)

instance Monad m => Zoom (BasePathStack s1 m) (BasePathStack s2 m) s1 s2 where
  zoom l = BasePathStack . zoom l . unBasePathStack

class (MonadReader s (ReaderM s m), MonadState s m) => ReadOnly s m where
  type ReaderM s m :: * -> *
  makeReadOnly :: ReaderM s m a -> m a

instance Monad m => ReadOnly s (BasePathStack s m) where
  type ReaderM s (BasePathStack s m) = ReaderT s m
  makeReadOnly = BasePathStack . readOnly

type PathStack err s = BasePathStack s (Either err)

class (MonadError FMCException m, Loggable m, MonadState s m) => PathStackable s m | m->s where
  asPathStack :: PathStack FMCException s a -> m a

instance PathStackable s (PathStack FMCException s) where
  asPathStack = id

newtype PPathStack s a =
  PPathStack { unPPathStack :: BasePathStack s (Producer LogEntry IO) a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadState s)

instance PathStackable s (PPathStack s) where
  asPathStack = PPathStack . hoist lift . hoist eitherToIO

runProducer :: BasePathStack s (Producer LogEntry IO) a -> BasePathStack s IO a
runProducer bpsp = BasePathStack $ StateT $ \x -> runEffect . (>-> printLog [minBound..]) $ runStateT (unBasePathStack bpsp) x

instance MonadError FMCException (PPathStack s) where
  throwError = throwM . toException -- because PPathStackR has a MonadThrow instance
  catchError action handler = PPathStack $ hoist lift $ MTC.control $ \run -> let f = run . runProducer in catch (f $ unPPathStack action) (f . unPPathStack . handler)

type instance Zoomed (PPathStack s) = Zoomed (StateT s (Producer LogEntry IO))

instance Zoom (PPathStack s1) (PPathStack s2) s1 s2 where
  zoom l = PPathStack . zoom l . unPPathStack

newtype PPathStackR s a =
  PPathStackR { unPPathStackR :: ReaderT s (Producer LogEntry IO) a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader s)

runProducerR :: ReaderT e (Producer LogEntry IO) a -> ReaderT e IO a
runProducerR r = ReaderT $ \x -> runEffect . (>-> printLog [minBound..]) $ runReaderT r x

instance MonadError FMCException (PPathStackR s) where
  throwError = throwM . toException -- because PPathStackR has a MonadThrow instance
  catchError action handler = PPathStackR $ hoist lift $ MTC.control $ \run -> let f = run . runProducerR in catch (f $ unPPathStackR action) (f . unPPathStackR . handler)

instance ReadOnly s (PPathStack s) where
  type ReaderM s (PPathStack s) = PPathStackR s
  makeReadOnly = PPathStack . BasePathStack . readOnly . unPPathStackR

execBasePathStack :: Monad m => BasePathStack s m a -> s -> m s
execBasePathStack bps s0 = execStateT (unBasePathStack bps) s0

execPathStack :: PathStack err s a -> s -> Either err s
execPathStack ps = execBasePathStack ps

execPPathStack :: PPathStack s a -> [LogLevel] -> s -> IO s
execPPathStack pps logDetails ps = runEffect . ( >-> printLog logDetails) $ execBasePathStack (unPPathStack pps) ps

class Loggable m where
  log :: LogLevel -> Text -> m ()

instance Loggable (PathStack err s) where
  log _ _ = return ()

instance Loggable (PPathStack s) where
  log ll = PPathStack . lift . faLog ll

instance (Monad m, Loggable m) => Loggable (ResultT o m) where
  log ll msg = lift $ log ll msg

faLog :: Monad m => LogLevel -> Text -> Producer LogEntry m ()
faLog l msg  = yield $ LogEntry l msg

printLog :: MonadIO m => [LogLevel] -> Proxy () LogEntry () X  m a
printLog lvls = do
  let printL x = do
        let lvl = x ^. leLevel
            msg = x ^. leMsg
            shouldLog = lvl `elem` lvls
        when shouldLog (liftIO $ putStrLn (show lvl ++ ": " ++ (unpack msg)))
  le <- await
  unless (null lvls) (printL le)
  printLog lvls

