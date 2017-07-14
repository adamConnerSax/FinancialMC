{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
       , HasPathState (..)
       , ReadsStepEnv (..)
       , Loggable (log)
       , execPathStack
       , execPPathStack
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
                                                   magnify, makeClassy, set,
                                                   zoom, (^.))

import           Control.Monad                    (unless, when)
import qualified Control.Monad.Base               as MB
import           Control.Monad.Catch              (MonadThrow (..),
                                                   SomeException, catch, throwM,
                                                   toException)
import           Control.Monad.Except             (MonadError (..))
import           Control.Monad.Morph              (generalize, hoist)
import           Control.Monad.Reader             (MonadReader, ReaderT, ask,
                                                   runReaderT)
import           Control.Monad.State.Strict       (MonadState, StateT,
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

data PathState s e = PathState { _stepState :: !s, _stepEnv :: !e } deriving (Show)
makeClassy ''PathState

class ReadsStepEnv s e where
  getEnv :: Getter (PathState s e) e
  default getEnv :: HasPathState s e e => Getter (PathState s e) e
  getEnv = stepEnv

newtype BasePathStack s e m a =
  BasePathStack { unBasePathStack :: StateT (PathState s e) m a }
  deriving (Functor, Applicative, Monad, MonadState (PathState s e))

{-
instance (Monad m, Applicative (BasePathStack s e m)) => Monad (BasePathStack s e m) where
  return = pure
  bpsa >>= f = BasePathStack $ (unBasePathStack bpsa) >>= unBasePathStack . f
-}

instance MFunctor (BasePathStack s e) where
  hoist f (BasePathStack bsa) = BasePathStack $ hoist f bsa

instance MonadTrans (BasePathStack s e) where
  lift  = BasePathStack . lift

instance MonadIO m => MonadIO (BasePathStack s e m) where
  liftIO = lift . liftIO

deriving instance MB.MonadBase b m => MB.MonadBase b (BasePathStack s e m)

instance MTC.MonadTransControl (BasePathStack s e) where
  type StT (BasePathStack s e) a = MTC.StT (StateT (PathState s e)) a
  liftWith = MTC.defaultLiftWith BasePathStack unBasePathStack
  restoreT = MTC.defaultRestoreT BasePathStack

instance MTC.MonadBaseControl b m => MTC.MonadBaseControl b (BasePathStack s e m) where
  type StM (BasePathStack s e m) a = MTC.ComposeSt (BasePathStack s e) m a
  liftBaseWith = MTC.defaultLiftBaseWith
  restoreM = MTC.defaultRestoreM

instance (MTC.MonadBaseControl m (BasePathStack s e m), MonadError err m) => MonadError err (BasePathStack s e m) where
  throwError = lift . throwError
  catchError action handler = MTC.control $ \run -> catchError (run action) (run . handler)

instance MonadThrow m => MonadThrow (BasePathStack s e m) where
  throwM = lift . throwM
{-
class ZoomState s1 s2 e f (m :: * -> *) where
  zoomState :: Lens' s1 s2 -> f s2 e m a -> f s1 e m a

instance (Monad m) => ZoomState s1 s2 e BasePathStack m where
  zoomState ls = BasePathStack . (zoom (stepState.ls))  . unBasePathStack
-}
newtype PathStack err s e a =
  PathStack { unPathStack :: BasePathStack s e (Either err) a } deriving (Functor, Applicative, Monad, MonadState (PathState s e))

instance MonadError err (PathStack err s e) where
  throwError = PathStack . throwError
  catchError action handler = PathStack $ catchError (unPathStack action) (unPathStack . handler)

newtype PPathStack s e a =
  PPathStack { unPPathStack :: Producer LogEntry (BasePathStack s e IO) a } deriving (Functor, Applicative, Monad, MonadState (PathState s e))

instance MonadIO (PPathStack s e) where
  liftIO  = PPathStack . lift . liftIO

instance MonadThrow (PPathStack s e) where
  throwM  = liftIO . throwM

instance MonadError FMCException (PPathStack s e) where
  throwError = throwM . toException -- because PPathStack has a MonadThrow instance
  catchError action handler = PPathStack $ do
    let ppsTobps = runEffect . (>-> printLog [minBound..]) . unPPathStack -- log it all.  It's an error, after all.
        ce a h = MTC.control $ \run -> catch (run a) (run . h) -- and use MonadBaseControl for the BasePathStack lifting
    lift $ ce (ppsTobps action) (ppsTobps . handler)

execBasePathStack :: Monad m => BasePathStack s e m a -> PathState s e -> m (PathState s e)
execBasePathStack bps s0 = execStateT (unBasePathStack bps) s0

execPathStack :: PathStack err s e a -> PathState s e -> Either err (PathState s e)
execPathStack ps = execBasePathStack (unPathStack ps)

execPPathStack :: PPathStack s e a -> [LogLevel] -> PathState s e -> IO (PathState s e)
execPPathStack pps logDetails = execBasePathStack (runEffect (unPPathStack pps >-> printLog logDetails))

class Loggable m where
  log :: LogLevel -> Text -> m ()


instance Loggable (PathStack err s e) where
  log _ _ = return ()

instance Loggable (PPathStack s e) where
  log ll = PPathStack . faLog ll

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

