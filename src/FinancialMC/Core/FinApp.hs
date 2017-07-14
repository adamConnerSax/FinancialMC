{-# LANGUAGE ConstraintKinds            #-}
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
--       , faLog
       , toStepApp
       , toPathApp
       , zoomStepApp
       , zoomStep
       , magnifyStepApp
       , zoomPathAppS
       , zoomPathAppE
       , Loggable (log)
       , LoggableStepApp (..)
       , LoggablePathApp (..)
       , StepApp
       , execPathApp
       , execPPathApp
       , taxDataApp2StepAppFSER
-- for bench/profiling
       , PathApp
       ) where

import           FinancialMC.Core.FinancialStates (FinEnv, FinState,
                                                   HasFinEnv (..),
                                                   HasFinState (..))
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction)
import           FinancialMC.Core.Tax             (TaxData, TaxDataApp)
import           FinancialMC.Core.Utilities       (AsFMCException, FMCException,
                                                   HasFMCException, eitherToIO,
                                                   multS, readOnly)

import           Control.Lens                     (Identity, Lens', magnify,
                                                   makeClassy, set, zoom, (^.))

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

newtype BaseStepApp s e m a =
  BaseStepApp { unBaseStepApp :: StateT s (ReaderT e m) a }
  deriving (Functor, Applicative, Monad, MonadReader e, MonadState s)

instance MFunctor (BaseStepApp s e) where
  hoist f (BaseStepApp bsa) = BaseStepApp $ hoist (hoist f) bsa

instance MonadTrans (BaseStepApp s e) where
  lift  = BaseStepApp . lift . lift

instance MonadIO m => MonadIO (BaseStepApp s e m) where
  liftIO = lift . liftIO

deriving instance MB.MonadBase b m => MB.MonadBase b (BaseStepApp s e m)

instance MTC.MonadTransControl (BaseStepApp s e) where
  type StT (BaseStepApp s e) a = MTC.StT (ReaderT e) (MTC.StT (StateT s) a)
  liftWith = MTC.defaultLiftWith2 BaseStepApp unBaseStepApp
  restoreT = MTC.defaultRestoreT2 BaseStepApp

instance MTC.MonadBaseControl b m => MTC.MonadBaseControl b (BaseStepApp s e m) where
  type StM (BaseStepApp s e m) a = MTC.ComposeSt (BaseStepApp s e) m a
  liftBaseWith = MTC.defaultLiftBaseWith
  restoreM = MTC.defaultRestoreM

instance (MTC.MonadBaseControl m (BaseStepApp s e m), MonadError err m) => MonadError err (BaseStepApp s e m) where
  throwError = lift . throwError
  catchError action handler = MTC.control $ \run -> catchError (run action) (run . handler)

instance MonadThrow m => MonadThrow (BaseStepApp s e m) where
  throwM = lift . throwM

zoomBaseStepApp :: Monad m => Lens' s2 s1 -> BaseStepApp s1 e m a -> BaseStepApp s2 e m a
zoomBaseStepApp l = BaseStepApp . (zoom l) . unBaseStepApp

magnifyBaseStepApp :: Monad m => Lens' e2 e1 -> BaseStepApp s e1 m a -> BaseStepApp s e2 m a
magnifyBaseStepApp l = BaseStepApp . (hoist $ magnify l) . unBaseStepApp

newtype StepApp err s e a =
  StepApp { unStepApp :: BaseStepApp s e (Either err) a } deriving (Functor, Applicative, Monad, MonadState s, MonadReader e)

instance MonadError err (StepApp err s e) where
  throwError = StepApp . throwError
  catchError action handler = StepApp $ catchError (unStepApp action) (unStepApp . handler)

--instance MonadThrow (StepApp SomeException s e) where
--  throwM  = StepApp . throwM

newtype PStepApp s e a =
  PStepApp { unPStepApp :: Producer LogEntry (BaseStepApp s e IO) a } deriving (Functor, Applicative, Monad, MonadState s, MonadReader e)
instance MonadIO (PStepApp s e) where
  liftIO  = PStepApp . lift . liftIO

instance MonadThrow (PStepApp s e) where
  throwM  = liftIO . throwM

instance MonadError FMCException (PStepApp s e) where
  throwError = throwM . toException -- because PStepApp has a MonadThrow instance
  catchError action handler = PStepApp $ do
    let psaTobsa = runEffect . (>-> printLog [minBound..]) . unPStepApp -- log it all.  It's an error, after all.
        ce a h = MTC.control $ \run -> catch (run a) (run . h) -- and use MonadBaseControl for the BaseStepApp lifting
    lift $ ce (psaTobsa action) (psaTobsa . handler)

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

execBasePathApp :: Monad m => BasePathApp s e m a -> s -> e -> m (s,e)
execBasePathApp bpa st env = execStateT (unBasePathApp bpa) (st,env)

execPathApp :: PathApp err s e a -> s -> e -> Either err (s,e)
execPathApp app = execBasePathApp (unPathApp app)

execPPathApp :: PPathApp s e a -> [LogLevel] -> s -> e -> IO (s,e)
execPPathApp app logDetails = execBasePathApp (runEffect (unPPathApp app >-> printLog logDetails))

liftStepApp :: StepApp FMCException s e a -> PStepApp s e a
liftStepApp = PStepApp . lift . hoist eitherToIO . unStepApp -- adds IO at bottom of stack and wraps in Producer

liftPathApp :: PathApp FMCException s e a -> PPathApp s e a
liftPathApp = PPathApp . lift . hoist eitherToIO . unPathApp

baseStep2basePath :: Monad m => BaseStepApp s e m a -> BasePathApp s e m a
baseStep2basePath = BasePathApp . multS . hoist readOnly . unBaseStepApp

class Loggable m where
  log :: LogLevel -> Text -> m ()

class (MonadError FMCException m, Loggable m, MonadState s m, MonadReader e m) => LoggableStepApp s e m where
  stepLift :: StepApp FMCException s e z -> m z

zoomStep :: LoggableStepApp s e m => Lens' s sInner -> StepApp FMCException sInner e x -> m x
zoomStep l = stepLift . zoomStepApp l

instance Loggable (StepApp err s e) where
  log _ _ = return ()

instance LoggableStepApp s e (StepApp FMCException s e) where
  stepLift = id

instance Loggable (PStepApp s e) where
  log ll = PStepApp . faLog ll

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

taxDataApp2StepAppFSER :: forall m a. LoggableStepApp FinState ExchangeRateFunction m
  => TaxDataApp (Either FMCException) a -> m a
taxDataApp2StepAppFSER x =
  let sa :: StepApp FMCException FinState ExchangeRateFunction a
      sa = zoomStepApp fsTaxData $ toStepApp $ x
  in stepLift sa

{-
taxDataApp2StepAppFS :: TaxDataApp (Either FMCException) a -> StepApp FMCException FinState (FinEnv rm) a
taxDataApp2StepAppFS tda = zoomStepApp fsTaxData . magnifyStepApp feExchange . toStepApp $ tda


taxDataApp2StepApp :: LoggableStepApp TaxData ExchangeRateFunction m => TaxDataApp (Either FMCException) a -> m a
taxDataApp2StepApp = stepLift . toStepApp

-}
