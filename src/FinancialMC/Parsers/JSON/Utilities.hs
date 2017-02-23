{-# LANGUAGE DeriveGeneric #-}
module FinancialMC.Parsers.JSON.Utilities
       (
         toJSONEnumMap
       , fromJSONEnumMap
       , EnumKeyMap(..)
       ) where

import           Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON), Value)
import           Data.Aeson.Types (Parser)

import qualified Data.Map         as M
import           Data.Maybe       (fromJust)
import GHC.Generics

enumFromString::(Show a,Enum a,Bounded a)=>String->a
enumFromString s =
  let enumValues = [minBound..]
      valueMap = (\x->(show x,x)) <$> enumValues
      in fromJust $ lookup s valueMap

toJSONEnumMap::(Show k, ToJSON a)=>M.Map k a->Value
toJSONEnumMap m = toJSON (M.mapKeys show m)

fromJSONEnumMap::(Show k,Enum k, Bounded k, Ord k, FromJSON a)=>Value->Parser (M.Map k a)
fromJSONEnumMap v = M.mapKeys enumFromString <$> parseJSON v

newtype EnumKeyMap k a = EnumKeyMap { unEnumKeyMap::M.Map k a } deriving (Show,Generic)

instance (Show k, ToJSON a)=>ToJSON (EnumKeyMap k a) where
  toJSON = toJSONEnumMap . unEnumKeyMap

instance (Show k, Enum k, Bounded k, Ord k, FromJSON a)=>FromJSON (EnumKeyMap k a) where
  parseJSON v = EnumKeyMap <$> fromJSONEnumMap v


