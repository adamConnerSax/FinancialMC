{-# LANGUAGE ConstraintKinds            #-}
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
       , LoggableStepApp
       , LoggablePathApp
       , StepLiftable (stepLift)
       , StepApp
       , execPathApp
       , execPPathApp
       ) where

import           FinancialMC.Core.FinancialStates (FinEnv, FinState,
                                                   HasFinEnv (..),
                                                   HasFinState (..))
import           FinancialMC.Core.MoneyValue      (ExchangeRateFunction)
import           FinancialMC.Core.Tax             (TaxData, TaxDataApp)
import           FinancialMC.Core.Utilities       (eitherToIO, multS, readOnly)

import           Control.Lens                     (Identity, Lens', magnify,
                                                   makeClassy, set, zoom, (^.))

import           Control.Monad                    (unless, when)
import           Control.Monad.Catch              (MonadThrow (..),
                                                   SomeException)
import           Control.Monad.Except             (MonadError (..))
import           Control.Monad.Morph              (generalize, hoist)
import           Control.Monad.Reader             (MonadReader, ReaderT, ask,
                                                   runReaderT)
import           Control.Monad.State.Strict       (MonadState, StateT,
                                                   execStateT, get, put,
                                                   runStateT)
import           Control.Monad.Trans              (MonadIO, MonadTrans, lift,
                                                   liftIO)
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
--  hoist f = fmap (hoist (hoist f))

instance MonadTrans (BaseStepApp s e) where
  lift  = BaseStepApp . lift . lift

instance MonadIO m => MonadIO (BaseStepApp s e m) where
  liftIO = lift . liftIO

-- this is all what MonadBaseControl is for, right??
-- also, this has semantics, when and if state is updated, etc.
instance MonadError err m => MonadError err (BaseStepApp s e m) where
  throwError = lift . throwError
  catchError action handler = do
    curState <- get
    curEnv <- ask
    let bsaToMonadError = flip runReaderT curEnv . flip runStateT curState . unBaseStepApp
    (a, newState)  <- lift $ do
      let action'  = bsaToMonadError action
          handler' = bsaToMonadError . handler
      catchError action' handler' -- this is happening in the MonadError err m
    put newState
    return a

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

instance MonadThrow (StepApp SomeException s e) where
  throwM  = StepApp . throwM

newtype PStepApp s e a =
  PStepApp { unPStepApp :: Producer LogEntry (BaseStepApp s e IO) a } deriving (Functor, Applicative, Monad, MonadState s, MonadReader e)

instance MonadThrow (PStepApp s e) where
  throwM  = PStepApp . lift . lift . throwM

instance MonadIO (PStepApp s e) where
  liftIO  = PStepApp . lift . liftIO


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

-- this is all what MonadBaseControl is for, right??
-- also, this has semantics, when and if state is updated, etc.
instance MonadError err m => MonadError err (BasePathApp s e m) where
  throwError = lift . throwError
  catchError action handler = do
    curState <- get
    let bpaToMonadError = flip runStateT curState . unBasePathApp
    (a, newState) <- lift $ do
      let action' = bpaToMonadError action
          handler' = bpaToMonadError . handler
      catchError action' handler'
    put newState
    return a

lensFirst :: Lens' a b -> Lens' (a,x) (b,x)
lensFirst l q (a,x) = (\(b,y) -> (set l b a,y)) <$> q (a ^. l,x)

zoomBasePathAppS :: Monad m => Lens' s2 s1 -> BasePathApp s1 e m a -> BasePathApp s2 e m a
zoomBasePathAppS l = BasePathApp . zoom (lensFirst l) . unBasePathApp  -- fmap (zoom (lensFirst l))

lensSecond::Lens' a b->Lens' (x,a) (x,b)
lensSecond l q (x,a) = (\(y,b)->(y,set l b a)) <$> q (x, a ^. l)

zoomBasePathAppE :: Monad m => Lens' e2 e1 -> BasePathApp s e1 m a -> BasePathApp s e2 m a
zoomBasePathAppE l = BasePathApp . zoom (lensSecond l) . unBasePathApp -- fmap (zoom (lensSecond l))

newtype PathApp err s e a =
  PathApp { unPathApp :: BasePathApp s e (Either err) a} deriving (Functor, Applicative, Monad, MonadState (s,e))

instance MonadError err (PathApp err s e) where
  throwError = PathApp . throwError
  catchError action handler = PathApp $ catchError (unPathApp action) (unPathApp . handler)

instance MonadThrow (PathApp SomeException s e) where
  throwM = PathApp . lift . throwM

newtype PPathApp s e a =
  PPathApp { unPPathApp :: Producer LogEntry (BasePathApp s e IO) a} deriving (Functor, Applicative, Monad, MonadState (s,e))

instance MonadThrow (PPathApp s e) where
  throwM = PPathApp . lift . lift . throwM

instance MonadIO (PPathApp s e) where
  liftIO = PPathApp . lift . liftIO

{-
instance MonadIO (BasePathApp s e IO) where
  liftIO  = BasePathApp . liftIO m

instance (MonadIO (BasePathApp s e IO)) => MonadIO (PPathApp s e) where
  liftIO m = PPathApp $ liftIO m
-}

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

liftStepApp :: StepApp SomeException s e a -> PStepApp s e a
liftStepApp = PStepApp . lift . hoist eitherToIO . unStepApp -- adds IO at bottom of stack and wraps in Producer

liftPathApp :: PathApp SomeException s e a -> PPathApp s e a
liftPathApp = PPathApp . lift . hoist eitherToIO . unPathApp

baseStep2basePath :: Monad m => BaseStepApp s e m a -> BasePathApp s e m a
baseStep2basePath = BasePathApp . multS . hoist readOnly . unBaseStepApp

class Loggable m where
  log :: LogLevel -> Text -> m ()

class StepLiftable err s e m where
  stepLift :: StepApp err s e a -> m a

class Monad n => BaseStepLiftable s e n m where
  baseStepLift :: BaseStepApp s e n a -> m a

type LoggableStepApp err s e m = (MonadThrow m, Loggable m, MonadState s m, MonadReader e m)

{-
class (MonadThrow m, MonadState s m, MonadReader e m) => LoggableStepApp s e m where
  stepLog :: LogLevel -> String -> m ()
  stepLift :: StepApp err s e z -> m z
  fromBaseS :: BaseStepApp s e Identity x -> m x
-}

zoomStep :: (LoggableStepApp err s e m, StepLiftable err s e m) => Lens' s sInner -> StepApp err sInner e x -> m x
zoomStep l = stepLift . zoomStepApp l

instance Loggable (StepApp err s e) where
  log _ _ = return ()

instance StepLiftable err s e (StepApp err s e) where
  stepLift = id

instance BaseStepLiftable s e Identity (StepApp err s e) where
  baseStepLift = StepApp . hoist generalize

{-
instance LoggableStepApp s e (StepApp err s e) where
  stepLog _ _ = return ()
  stepLift = id
  fromBaseS = StepApp . hoist generalize
-}

instance Loggable (PStepApp s e) where
  log ll = PStepApp . faLog ll

instance StepLiftable SomeException s e (PStepApp s e) where
  stepLift = liftStepApp

instance BaseStepLiftable s e Identity (PStepApp s e) where
  baseStepLift = PStepApp . lift . hoist generalize

{-
instance LoggableStepApp s e (PStepApp s e) where
  stepLog ll msg = PStepApp $ faLog ll msg
  stepLift = liftStepApp
  fromBaseS = PStepApp . lift . hoist generalize
-}
class PathLiftable err s e m where
  pathLift :: PathApp err s e a -> m a

class Monad n => BasePathLiftable s e n m where
  basePathLift :: BasePathApp s e n a -> m a

class StepToPath stepM pathM where
  stepToPath :: stepM a -> pathM a

type LoggablePathApp err s e m = (MonadThrow m, Loggable m, MonadState (s,e) m)

{-
class (MonadThrow m, MonadState (s,e) m) => LoggablePathApp s e m where
  type Step s e m :: * -> *
  pathLog :: LogLevel -> String -> m ()
  pathLift :: PathApp err s e z -> m z
  fromBaseP :: BasePathApp s e Identity x -> m x
  step2Path :: Step s e m x -> m x
-}

instance Loggable (PathApp err s e) where
  log _ _ = return ()

instance PathLiftable err s e (PathApp err s e) where
  pathLift = id

instance BasePathLiftable s e Identity (PathApp err s e) where
  basePathLift = PathApp . hoist generalize

instance StepToPath (StepApp err s e) (PathApp err s e) where
  stepToPath = PathApp . baseStep2basePath . unStepApp

{-
instance LoggablePathApp s e (PathApp err s e) where
  type Step s e (PathApp err s e)  = StepApp err s e
  pathLog _ _ = return ()
  pathLift = id
  fromBaseP = PathApp . hoist generalize
  step2Path = PathApp . baseStep2basePath . unStepApp
-}

instance Loggable (PPathApp s e) where
  log ll = PPathApp . faLog ll

instance PathLiftable SomeException s e (PPathApp s e) where
  pathLift = liftPathApp

instance BasePathLiftable s e Identity (PPathApp s e) where
  basePathLift = PPathApp . lift . hoist generalize

instance StepToPath (PStepApp s e) (PPathApp s e) where
  stepToPath = PPathApp . hoist baseStep2basePath . unPStepApp

{-
instance LoggablePathApp s e (PPathApp s e) where
  type Step s e (PPathApp s e) = PStepApp s e
  pathLog ll = PPathApp . faLog ll
  pathLift = liftPathApp
  fromBaseP = PPathApp . lift . hoist generalize
  step2Path = PPathApp . hoist baseStep2basePath . unPStepApp
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

--taxDataApp2StepAppFSER::LoggableStepApp FinState ExchangeRateFunction app => TaxDataApp (Either SomeException) a->app a
taxDataApp2StepAppFSER :: forall err m a. StepLiftable err FinState ExchangeRateFunction m => TaxDataApp (Either err) a -> m a
taxDataApp2StepAppFSER x =
  let sa :: StepApp err FinState ExchangeRateFunction a
      sa = zoomStepApp fsTaxData . toStepApp $ x
  in stepLift sa

taxDataApp2StepAppFS :: TaxDataApp (Either err) a -> StepApp err FinState (FinEnv rm) a
taxDataApp2StepAppFS tda = zoomStepApp fsTaxData . magnifyStepApp feExchange . toStepApp $ tda

--taxDataApp2StepApp :: LoggableStepApp TaxData ExchangeRateFunction app=>TaxDataApp (Either SomeException) a->app a
taxDataApp2StepApp :: StepLiftable err TaxData ExchangeRateFunction m => TaxDataApp (Either err) a -> m a
taxDataApp2StepApp = stepLift . toStepApp
