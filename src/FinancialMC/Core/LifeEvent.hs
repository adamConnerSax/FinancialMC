{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
module FinancialMC.Core.LifeEvent
       (
         LifeEventName
       , LifeEventCore(..)
       , IsLifeEvent(..)
       , LifeEventConverters(LEC)
       , lifeEventName
       , lifeEventYear
       , LifeEventOutput(..)
       , LifeEventApp
       ) where

import           FinancialMC.Core.Asset           (Account, AccountGetter,
                                                   IsAsset)
import           FinancialMC.Core.FinancialStates (FinEnv, FinState)
import           FinancialMC.Core.Flow            (IsFlow)
import           FinancialMC.Core.Rates           (IsRateModel)
import           FinancialMC.Core.Result          (ResultT)
import           FinancialMC.Core.Utilities       (Year)

import           Control.Exception                (SomeException)
import           Control.Monad.Reader             (ReaderT)
import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   genericParseJSON,
                                                   genericToJSON)
import           Data.Aeson.Types                 (defaultOptions,
                                                   fieldLabelModifier)
import           Data.Bifunctor                   (Bifunctor (..))
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)

data LifeEventOutput a fl = LifeEventOutput ![Account a] ![fl]
instance Monoid (LifeEventOutput a fl) where
  mempty = LifeEventOutput mempty mempty
  mappend (LifeEventOutput a1 f1) (LifeEventOutput a2 f2) = LifeEventOutput (a1<>a2) (f1<>f2)

instance Bifunctor LifeEventOutput where
  first f (LifeEventOutput accts flows) = LifeEventOutput (fmap f <$> accts) flows
  second f (LifeEventOutput accts flows) = LifeEventOutput accts (f <$> flows)

type LifeEventApp a fl rm = ResultT (LifeEventOutput a fl) (ReaderT FinState (ReaderT (FinEnv rm) (Either SomeException)))

type LifeEventName = T.Text

{- Each instance fixes the AssetType.  If the AssetType in the engine is different, it needs to be mapped in the Result
 This Seems bad, maybe.
 The alternative would be to create the LifeEvent with return-type flexibility somehow
-}

data LifeEventCore = LifeEventCore { leName :: !T.Text, leYear :: !Year } deriving (Generic)
instance ToJSON LifeEventCore where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2 }

instance FromJSON LifeEventCore where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2 }

data LifeEventConverters a fl le = LEC (IsLifeEvent le=>AssetType le->a) (IsLifeEvent le=>FlowType le -> fl)

class IsLifeEvent e where
  type AssetType e :: *
  type FlowType e :: *
  lifeEventCore::e->LifeEventCore
  -- do we need/want the constraints here
  doLifeEvent::(IsAsset a,IsFlow fl,IsRateModel rm)=>e->LifeEventConverters a fl e->AccountGetter a->LifeEventApp a fl rm ()

lifeEventName::IsLifeEvent e=>e->LifeEventName
lifeEventName = leName . lifeEventCore

lifeEventYear::IsLifeEvent e=>e->Year
lifeEventYear = leYear . lifeEventCore


