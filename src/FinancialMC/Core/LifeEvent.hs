{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
--{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}
module FinancialMC.Core.LifeEvent
       (
         LifeEventName
       , LifeEventCore(..)
       , IsLifeEvent(..)
       , lifeEventName
       , lifeEventYear
       , LifeEventOutput(..)
       , LifeEventApp
       ) where

import           FinancialMC.Core.Asset           (Account, AccountGetter,
                                                   IsAsset)
import           FinancialMC.Core.FinancialStates (FinEnv, FinState)
import           FinancialMC.Core.Flow            (Flow)
import           FinancialMC.Core.Result          (ResultT)
import           FinancialMC.Core.Utilities       (Year)

import           Control.Exception                (SomeException)
import           Control.Monad.Reader             (ReaderT)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T

data LifeEventOutput a = LifeEventOutput ![Account a] ![Flow]
instance Monoid (LifeEventOutput a) where
  mempty = LifeEventOutput mempty mempty
  mappend (LifeEventOutput a1 f1) (LifeEventOutput a2 f2) = LifeEventOutput (a1<>a2) (f1<>f2)

instance Functor LifeEventOutput where
  fmap f (LifeEventOutput accts flows) = LifeEventOutput (fmap f <$> accts) flows

type LifeEventApp a = ResultT (LifeEventOutput a) (ReaderT FinState (ReaderT FinEnv (Either SomeException)))

type LifeEventName = T.Text

{- Each instance fixes the AssetType.  If the AssetType in the engine is different, it needs to be mapped in the Result
 This Seems bad, maybe.
 The alternative would be to create the LifeEvent with return-type flexibility somehow
-}

data LifeEventCore = LifeEventCore { leName :: !T.Text, leYear :: !Year }

class IsLifeEvent e where
  type AssetType e :: *
  lifeEventCore::e->LifeEventCore                    
  doLifeEvent::IsAsset a=>e->(AssetType e->a)->AccountGetter a->LifeEventApp a ()

lifeEventName::IsLifeEvent e=>e->LifeEventName
lifeEventName = leName . lifeEventCore

lifeEventYear::IsLifeEvent e=>e->Year
lifeEventYear = leYear . lifeEventCore


{-
data LifeEvent where
  MkLifeEvent::(IsLifeEvent e a, Show e, ToJSON e)=>e->LifeEvent

instance Show LifeEvent where
  show (MkLifeEvent le) = show le

instance TypeNamed LifeEvent where
  typeName (MkLifeEvent e) = typeName e

instance IsLifeEvent LifeEvent a where
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
-}
