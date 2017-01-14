{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
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


class (IsAsset a,TypeNamed (e a)) => IsLifeEvent e a where
  lifeEventName::e a->LifeEventName
  lifeEventYear::e a->Year
  doLifeEvent::e a->AccountGetter a->LifeEventApp a ()

data LifeEvent a where
  MkLifeEvent::(IsLifeEvent e a, Show (e a), ToJSON (e a))=>(e a)->LifeEvent a

instance Show a=>Show (LifeEvent a) where
  show (MkLifeEvent le) = show le

instance TypeNamed (LifeEvent a) where
  typeName (MkLifeEvent e) = typeName e

instance IsAsset a=>IsLifeEvent LifeEvent a where
  lifeEventName (MkLifeEvent e) = lifeEventName e
  lifeEventYear (MkLifeEvent e) = lifeEventYear e
  doLifeEvent (MkLifeEvent e) = doLifeEvent e


instance JSON_Existential (LifeEvent a) where
  containedName (MkLifeEvent le) = typeName le
  containedJSON (MkLifeEvent le) = toJSON le

instance ToJSON (LifeEvent a) where
  toJSON = existentialToJSON

instance HasParsers e (LifeEvent a) => EnvFromJSON e (LifeEvent a) where
  envParseJSON = parseJSON_Existential

