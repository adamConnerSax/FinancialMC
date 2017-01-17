{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies #-}
module FinancialMC.Core.LifeEvent
       (
         LifeEventName,
         LifeEvent(..)
       , IsLifeEvent(..)
       , LifeEventOutput(..)
       , LifeEventApp
       ) where

import           Data.Aeson                       (ToJSON (..))
import           FinancialMC.Core.Asset           (Account, AccountGetter,
                                                   IsAsset)
import           FinancialMC.Core.FinancialStates (FinEnv, FinState)
import           FinancialMC.Core.Flow            (Flow)
import           FinancialMC.Core.Result          (ResultT)
import           FinancialMC.Core.Utilities       (Year)

import           Data.Aeson.Existential           (HasParsers,
                                                   JSON_Existential (..),
                                                   TypeNamed (..),
                                                   existentialToJSON,
                                                   parseJSON_Existential)
import           Data.Aeson.Existential.EnvParser (EnvFromJSON (..))

import           Control.Exception                (SomeException)
import           Control.Monad.Reader             (ReaderT)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

data LifeEventOutput a = LifeEventOutput ![Account a] ![Flow]
instance Monoid (LifeEventOutput a) where
  mempty = LifeEventOutput mempty mempty
  mappend (LifeEventOutput a1 f1) (LifeEventOutput a2 f2) = LifeEventOutput (a1<>a2) (f1<>f2)

type LifeEventApp a = ResultT (LifeEventOutput a) (ReaderT FinState (ReaderT FinEnv (Either SomeException)))

type LifeEventName = T.Text


class TypeNamed e => IsLifeEvent e where
  lifeEventName::e->LifeEventName
  lifeEventYear::e->Year
  doLifeEvent::(IsAsset a, IsAsset b)=>e->AccountGetter b->LifeEventApp a ()

data LifeEvent where
  MkLifeEvent::(IsLifeEvent e, Show e, ToJSON e)=>e->LifeEvent

instance Show LifeEvent where
  show (MkLifeEvent le) = show le

instance TypeNamed LifeEvent where
  typeName (MkLifeEvent e) = typeName e

instance IsLifeEvent LifeEvent where
  lifeEventName (MkLifeEvent e) = lifeEventName e
  lifeEventYear (MkLifeEvent e) = lifeEventYear e
  doLifeEvent (MkLifeEvent e) = doLifeEvent e


instance JSON_Existential LifeEvent where
  containedName (MkLifeEvent le) = typeName le
  containedJSON (MkLifeEvent le) = toJSON le

instance ToJSON LifeEvent where
  toJSON = existentialToJSON

instance HasParsers e LifeEvent => EnvFromJSON e LifeEvent where
  envParseJSON = parseJSON_Existential

