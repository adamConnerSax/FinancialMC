{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module FinancialMC.Core.LifeEvent
       (
         LifeEventName,
         LifeEvent(..)
       , IsLifeEvent(..)
       , LifeEventOutput(..)
       , LifeEventApp
       ) where

import Data.Aeson (ToJSON(..))
import FinancialMC.Core.Asset (Account,AccountGetter)
import FinancialMC.Core.Flow (Flow)
import FinancialMC.Core.FinancialStates (FinState,FinEnv)
import FinancialMC.Core.Result (ResultT)
import FinancialMC.Core.Utilities (Year)

import Data.Aeson.Existential (TypeNamed(..),JSON_Existential(..),existentialToJSON,parseJSON_Existential,HasParsers)
import Data.Aeson.Existential.EnvParser (EnvFromJSON(..))

import Control.Exception (SomeException)
import Control.Monad.Reader (ReaderT)
import Data.Monoid ((<>))
import qualified Data.Text as T

data LifeEventOutput = LifeEventOutput ![Account] ![Flow]
instance Monoid LifeEventOutput where
  mempty = LifeEventOutput mempty mempty
  mappend (LifeEventOutput a1 f1) (LifeEventOutput a2 f2) = LifeEventOutput (a1<>a2) (f1<>f2)
  
type LifeEventApp = ResultT LifeEventOutput (ReaderT FinState (ReaderT FinEnv (Either SomeException)))

type LifeEventName = T.Text

class TypeNamed e=> IsLifeEvent e where
  lifeEventName::e->LifeEventName
  lifeEventYear::e->Year
  doLifeEvent::e->AccountGetter->LifeEventApp ()
  
data LifeEvent where
  MkLifeEvent::(IsLifeEvent e, Show e, ToJSON e)=>e->LifeEvent
  
instance Show LifeEvent where
  show (MkLifeEvent e) = show e
  
instance TypeNamed LifeEvent where
  typeName (MkLifeEvent e) = typeName e
  
instance IsLifeEvent LifeEvent where 
  lifeEventName (MkLifeEvent e) = lifeEventName e
  lifeEventYear (MkLifeEvent e) = lifeEventYear e
  doLifeEvent (MkLifeEvent e) = doLifeEvent e
  

instance JSON_Existential LifeEvent where
  containedName (MkLifeEvent a) = typeName a
  containedJSON (MkLifeEvent a) = toJSON a

instance ToJSON LifeEvent where
  toJSON = existentialToJSON 

instance HasParsers e LifeEvent => EnvFromJSON e LifeEvent where
  envParseJSON = parseJSON_Existential

