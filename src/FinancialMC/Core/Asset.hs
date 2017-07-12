{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
module FinancialMC.Core.Asset
       (
         TradeType
       , TradeFunction
       , TradeResult
--       , TradeApp
       , AssetRevaluation(..)
       , AssetName
       , AssetCore(..)
       , revalueAssetCore
       , IsAsset(..)
       , assetName
       , assetValue
       , assetCostBasis
       , assetCurrency
       , AccountName
       , Account(Account)
       , HasAccount(..)
       , accountValue
       , accountValueCV
       , AccountGetter
       ) where


import           FinancialMC.Core.CValued       ((|+|))
import qualified FinancialMC.Core.CValued       as CV
import           FinancialMC.Core.Evolve        (Evolvable (..), evolveWithin)
import           FinancialMC.Core.MoneyValue    (Currency (..),
                                                 ExchangeRateFunction,
                                                 MoneyValue (..), mCurrency)
import qualified FinancialMC.Core.MoneyValueOps as MV
import           FinancialMC.Core.Result        ()
import           FinancialMC.Core.TradingTypes  (AccountType, TradeAppC,
                                                 TradeFunction, TradeResult,
                                                 TradeType)
import           FinancialMC.Core.Utilities     (FMCException)

import           Control.Lens                   (makeClassy, (^.))
import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 genericParseJSON,
                                                 genericToJSON)
import           Data.Aeson.Types               (Options (fieldLabelModifier),
                                                 defaultOptions)

import qualified Data.Text                      as T

import           Control.Monad.Except           (MonadError)

import           GHC.Generics                   (Generic)

data AssetRevaluation = NewValue !MoneyValue | NewBasis !MoneyValue | NewValueAndBasis !MoneyValue !MoneyValue

type AssetName = T.Text

data AssetCore = AssetCore { aName:: !AssetName, aValue:: !MoneyValue, aCostBasis:: !MoneyValue } deriving (Generic)
instance Show AssetCore where
  show (AssetCore n v cb) = show n ++ ": value=" ++ show v ++ " (cost basis of " ++ show cb ++ ")"

instance ToJSON AssetCore where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier= drop 1}
instance FromJSON AssetCore where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier= drop 1}

revalueAssetCore::AssetCore->AssetRevaluation->AssetCore
revalueAssetCore (AssetCore n _ cb) (NewValue v') = AssetCore n v' cb
revalueAssetCore (AssetCore n v _) (NewBasis cb') = AssetCore n v cb'
revalueAssetCore (AssetCore n _ _) (NewValueAndBasis v' cb') = AssetCore n v' cb'

class Evolvable a => IsAsset a where
  assetCore :: a -> AssetCore
  revalueAsset :: a -> AssetRevaluation -> a
  tradeAsset :: TradeFunction s m a

assetName :: IsAsset a => a -> AssetName
assetName a = aName $ assetCore a

assetValue :: IsAsset a => a -> MoneyValue
assetValue a = aValue $ assetCore a

assetCostBasis :: IsAsset a => a -> MoneyValue
assetCostBasis a = aCostBasis $ assetCore a

assetCurrency::IsAsset a => a -> Currency
assetCurrency a = assetValue a ^. mCurrency

type AccountName = T.Text

data Account a = Account { _acName:: !AccountName,  _acType:: !AccountType, _acCurrency:: !Currency, _acAssets:: ![a] } deriving (Generic)
makeClassy ''Account

instance Functor Account where
  fmap f (Account n t c as) = Account n t c (f <$> as)

instance ToJSON a => ToJSON (Account a) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

instance FromJSON a => FromJSON (Account a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance Show a => Show (Account a) where
  show (Account n aType ccy as) = show n ++ "(" ++ show aType ++ " in " ++ show ccy ++ ")"
                               ++ foldr (\a s->s ++ "\n\t" ++ show a) " " as

instance Evolvable a => Evolvable (Account a) where
  evolve act = evolveWithin act acAssets


accountValue :: IsAsset a => Account a -> ExchangeRateFunction -> MoneyValue
accountValue acct e = foldr f (MV.zero ccy) assets where
  f a s = MV.inFirst e (+) s (assetValue a)
  ccy = acct ^. acCurrency
  assets = acct ^. acAssets


--accountValueCEMV::(Applicative m,Monad m)=>Account->MV.CER m MoneyValue
--accountValueCEMV acct = foldr f MV.cer0 (acct ^. acAssets) where
--  f a s = s |+| (MV.mv2cer $ assetValue a)

accountValueCV :: IsAsset a => Account a -> CV.CVD
accountValueCV acct = foldr f (CV.mvZero (acct ^. acCurrency))  (acct ^. acAssets) where
  f a s = s |+| (CV.fromMoneyValue $ assetValue a)

type AccountGetter m a = MonadError FMCException m => AccountName -> m (Account a)
