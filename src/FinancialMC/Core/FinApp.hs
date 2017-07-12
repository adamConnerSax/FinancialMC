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
--       , faLog
{-
       , toStepApp
       , toPathApp
       , zoomStepApp
       , zoomStep
       , magnifyStepApp
       , zoomPathAppS
       , zoomPathAppE
       , LoggableStepApp (..)
       , LoggablePathApp (..)
       , StepApp
-}
       , execPathStack
       , execPPathStack
--       , taxDataApp2StepAppFSER
       ) where

import           Prelude                          hiding (log)

import           FinancialMC.Core.FinancialStates (FinEnv, FinState,
                                                   HasFinEnv (..),
                                                   HasFinState (..))
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction)
--import           FinancialMC.Core.Tax             (TaxData, TaxDataApp)
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

data PathState s e = PathState { _stepState :: s, _stepEnv :: e }
makeClassy ''PathState

class ReadsStepEnv s e where
  getEnv :: Getter (PathState s e) e
  default getEnv :: HasPathState s e e => Getter (PathState s e) e
  getEnv = stepEnv

newtype BasePathStack s e m a =
  BasePathStack { unBasePathStack :: StateT (PathState s e) m a }
  deriving (Functor, Applicative, Monad, MonadState (PathState s e))

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
zoomBaseStepApp :: Monad m => Lens' s2 s1 -> BaseStepApp s1 e m a -> BaseStepApp s2 e m a
zoomBaseStepApp l = BaseStepApp . (zoom l) . unBaseStepApp

magnifyBaseStepApp :: Monad m => Lens' e2 e1 -> BaseStepApp s e1 m a -> BaseStepApp s e2 m a
magnifyBaseStepApp l = BaseStepApp . (hoist $ magnify l) . unBaseStepApp
-}

newtype PathStack err s e a =
  PathStack { unPathStack :: BasePathStack s e (Either err) a } deriving (Functor, Applicative, Monad, MonadState (PathState s e))

instance MonadError err (PathStack err s e) where
  throwError = PathStack . throwError
  catchError action handler = PathStack $ catchError (unPathStack action) (unPathStack . handler)

--instance MonadThrow (StepApp SomeException s e) where
--  throwM  = StepApp . throwM

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

{-
zoomStepApp :: Lens' s2 s1 -> StepApp err s1 e a -> StepApp err s2 e a
zoomStepApp l = StepApp . (zoomBaseStepApp l) . unStepApp

magnifyStepApp :: Lens' e2 e1 -> StepApp err s e1 a -> StepApp err s e2 a
magnifyStepApp l = StepApp . (magnifyBaseStepApp l) . unStepApp

toStepApp :: StateT s (ReaderT e (Either err)) a -> StepApp err s e a
toStepApp  = StepApp . BaseStepApp

newtype BasePathApp s e m a =
  BasePathApp { unBasePathApp :: StateT (s,e) m a }
  deriving (Functor, Applicative, Monad, MonadState (s,e), MFunctor, MonadTrans)

instance MonadIO m => MonadIO (BasePathApp s e m) where
  liftIO = lift . liftIO

deriving instance MB.MonadBase b m => MB.MonadBase b (BasePathApp s e m)

instance MTC.MonadTransControl (BasePathApp s e) where
  type StT (BasePathApp s e) a = MTC.StT (StateT (s,e)) a
  liftWith = MTC.defaultLiftWith BasePathApp unBasePathApp
  restoreT = MTC.defaultRestoreT BasePathApp

instance MTC.MonadBaseControl b m => MTC.MonadBaseControl b (BasePathApp s e m) where
  type StM (BasePathApp s e m) a = MTC.ComposeSt (BasePathApp s e) m a
  liftBaseWith = MTC.defaultLiftBaseWith
  restoreM = MTC.defaultRestoreM

instance (MTC.MonadBaseControl m (BasePathApp s e m), MonadError err m) => MonadError err (BasePathApp s e m) where
  throwError = lift . throwError
  catchError action handler = MTC.control $ \run -> catchError (run action) (run . handler)

lensFirst :: Lens' a b -> Lens' (a,x) (b,x)
lensFirst l q (a,x) = (\(b,y) -> (set l b a,y)) <$> q (a ^. l,x)

zoomBasePathAppS :: Monad m => Lens' s2 s1 -> BasePathApp s1 e m a -> BasePathApp s2 e m a
zoomBasePathAppS l = BasePathApp . zoom (lensFirst l) . unBasePathApp  -- fmap (zoom (lensFirst l))

lensSecond :: Lens' a b->Lens' (x,a) (x,b)
lensSecond l q (x,a) = (\(y,b)->(y,set l b a)) <$> q (x, a ^. l)

zoomBasePathAppE :: Monad m => Lens' e2 e1 -> BasePathApp s e1 m a -> BasePathApp s e2 m a
zoomBasePathAppE l = BasePathApp . zoom (lensSecond l) . unBasePathApp -- fmap (zoom (lensSecond l))

newtype PathApp err s e a =
  PathApp { unPathApp :: BasePathApp s e (Either err) a} deriving (Functor, Applicative, Monad, MonadState (s,e))

instance MonadError err (PathApp err s e) where
  throwError = PathApp . throwError
  catchError action handler = PathApp $ catchError (unPathApp action) (unPathApp . handler)

instance MonadIO (PPathApp s e) where
  liftIO = PPathApp . lift . liftIO

--instance MonadThrow (PathApp SomeException s e) where
--  throwM = PathApp . lift . throwM

newtype PPathApp s e a =
  PPathApp { unPPathApp :: Producer LogEntry (BasePathApp s e IO) a} deriving (Functor, Applicative, Monad, MonadState (s,e))

instance MonadThrow (PPathApp s e) where
  throwM = liftIO . throwM

instance MonadError FMCException (PPathApp s e) where
  throwError = throwM . toException -- because PPathApp has a MonadThrow instance
  catchError action handler = PPathApp $ do
    let ppaTobpa = runEffect . (>-> printLog [minBound..]) . unPPathApp -- log it all.  It's an error, right?
        ce a h = MTC.control $ \run -> catch (run a) (run . h) -- use MonadBaseControl to deal with lifiting in PPathApp
    lift $ ce (ppaTobpa action) (ppaTobpa . handler)

zoomPathAppS :: Lens' s2 s1 -> PathApp err s1 e a -> PathApp err s2 e a
zoomPathAppS l = PathApp . (zoomBasePathAppS l) . unPathApp

zoomPathAppE :: Lens' e2 e1 -> PathApp err s e1 a -> PathApp err s e2 a
zoomPathAppE l = PathApp . (zoomBasePathAppE l) . unPathApp

toPathApp :: StateT (s,e) (Either err) a -> PathApp err s e a
toPathApp = PathApp . BasePathApp


liftPathApp :: PathApp FMCException s e a -> PPathApp s e a
liftPathApp = PPathApp . lift . hoist eitherToIO . unPathApp

baseStep2basePath :: Monad m => BaseStepApp s e m a -> BasePathApp s e m a
baseStep2basePath = BasePathApp . multS . hoist readOnly . unBaseStepApp
-}

addProducer :: PathStack FMCException s e a -> PPathStack s e a
addProducer = PPathStack . lift . hoist eitherToIO . unPathStack -- adds IO at bottom of stack and wraps in Producer

execBasePathStack :: Monad m => BasePathStack s e m a -> PathState s e -> m (PathState s e)
execBasePathStack bps s0 = execStateT (unBasePathStack bps) s0

execPathStack :: PathStack err s e a -> PathState s e -> Either err (PathState s e)
execPathStack ps = execBasePathStack (unPathStack ps)

execPPathStack :: PPathStack s e a -> [LogLevel] -> PathState s e -> IO (PathState s e)
execPPathStack pps logDetails = execBasePathStack (runEffect (unPPathStack pps >-> printLog logDetails))

class Loggable m where
  log :: LogLevel -> Text -> m ()

{-
class (MonadError FMCException m, Loggable m, MonadState s m, MonadReader e m) => LoggableStepApp s e m where
  stepLift :: StepApp FMCException s e z -> m z

zoomStep :: LoggableStepApp s e m => Lens' s sInner -> StepApp FMCException sInner e x -> m x
zoomStep l = stepLift . zoomStepApp l
-}

instance Loggable (PathStack err s e) where
  log _ _ = return ()

{-
instance LoggableStepApp s e (StepApp FMCException s e) where
  stepLift = id
-}

instance Loggable (PPathStack s e) where
  log ll = PPathStack . faLog ll

instance (Monad m, Loggable m) => Loggable (ResultT o m) where
  log ll msg = lift $ log ll msg

{-
instance LoggableStepApp s e (PStepApp s e) where
  stepLift = liftStepApp

class (MonadError FMCException m, MonadState (s,e) m) => LoggablePathApp s e m where
  type Step s e m :: * -> *
  pathLift :: PathApp FMCException s e a -> m a
  stepToPath :: Step s e m a -> m a

instance Loggable (PathApp FMCException s e) where
  log _ _ = return ()

instance LoggablePathApp s e (PathApp FMCException s e) where
  type Step s e (PathApp FMCException s e)  = StepApp FMCException s e
  pathLift = id
  stepToPath = PathApp . baseStep2basePath . unStepApp

instance Loggable (PPathApp s e) where
  log ll = PPathApp . faLog ll

instance LoggablePathApp s e (PPathApp s e) where
  type Step s e (PPathApp s e) = PStepApp s e
  pathLift = liftPathApp
  stepToPath = PPathApp . hoist baseStep2basePath . unPStepApp
-}

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

--taxDataApp2StepAppFSER :: (HasTaxData s, ReadsExchangeRateFunction s, MonadState s m) => m a

{-
taxDataApp2StepAppFSER :: forall m a. LoggableStepApp FinState ExchangeRateFunction m
  => TaxDataApp (Either FMCException) a -> m a
taxDataApp2StepAppFSER x =
  let sa :: StepApp FMCException FinState ExchangeRateFunction a
      sa = zoomStepApp fsTaxData $ toStepApp $ x
  in stepLift sa


taxDataApp2StepAppFS :: TaxDataApp (Either FMCException) a -> StepApp FMCException FinState (FinEnv rm) a
taxDataApp2StepAppFS tda = zoomStepApp fsTaxData . magnifyStepApp feExchange . toStepApp $ tda


taxDataApp2StepApp :: LoggableStepApp TaxData ExchangeRateFunction m => TaxDataApp (Either FMCException) a -> m a
taxDataApp2StepApp = stepLift . toStepApp

-}
