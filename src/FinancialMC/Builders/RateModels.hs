{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module FinancialMC.Builders.RateModels (
    IsRateModelFactor
  , BaseRateModelFactor (Fixed,Normal)
  , makeLogNormalFactor
  , BaseRateModel(..)
  , BaseRateModelT -- for ease of use
  , RateModelFactorC
  ) where

import           FinancialMC.Core.Rates     (IsRateModel (..), RSource, Rate,
                                             RateModelC, RateTable (..),
                                             RateTag (..), RateType (..),
                                             RateType (..), ReturnType (Stock),
                                             applyModel, defaultRateTable,
                                             isRateType, showRateAsPct)
import           FinancialMC.Core.Utilities (eitherToIO)


import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (MonadState, State, get, put,
                                             runState)
import qualified Data.Random                as Rand
import           Data.Random.Source.PureMT  (newPureMT)
import           GHC.Generics               (Generic)

import           Data.Aeson                 (FromJSON (parseJSON),
                                             ToJSON (toJSON), Value (Object),
                                             genericToJSON, object, (.:), (.=))

import           Data.Aeson.Types           (Options (..), defaultOptions)
import           Data.Maybe                 (fromMaybe)

type RateModelFactorC m = (MonadState RSource m, Rand.MonadRandom m)

type BaseRateModelT = BaseRateModel BaseRateModelFactor

-- factors
class IsRateModelFactor a where
  rateModelFactorF::RateModelFactorC m=>a->m (Rate,a)


data BaseRateModelFactor = Fixed !Rate |
                           Normal !Rate !Rate |
                           LogNormal !Rate !Rate !(Maybe (Double,Double)) deriving (Generic,ToJSON,FromJSON)

instance Show BaseRateModelFactor where
  show (Fixed rate) = "Fixed: " ++ showRateAsPct rate
  show (Normal mean var) = "Normal: mean=" ++ showRateAsPct mean ++ "; var=" ++ showRateAsPct var
  show (LogNormal mean var _) = "LogNormal: mean=" ++ showRateAsPct mean ++ "; var=" ++ showRateAsPct var

instance IsRateModelFactor BaseRateModelFactor where
  rateModelFactorF f@(Fixed rate) = return (rate, f)
  rateModelFactorF n@(Normal mean var) = (,n) <$> Rand.sample (Rand.Normal mean var)
  rateModelFactorF (LogNormal mean var mMuS) = logNormalRateModelF mean var mMuS

-- smart constructor
makeLogNormalFactor :: Double -> Double -> BaseRateModelFactor
makeLogNormalFactor mean var = LogNormal mean var Nothing

logNormalParams :: Double -> Double -> (Double, Double)
logNormalParams mean vol = (mu,s) where
  m2 = mean*mean
  v = vol*vol
  mu = log (m2/sqrt (v+m2))
  s = sqrt (log (1.0 + (v/m2)))

logNormalRateModelF :: RateModelFactorC m => Double -> Double -> Maybe (Double,Double) -> m (Rate,BaseRateModelFactor)
logNormalRateModelF mean vol mMuS= do
  let (mu,s) = fromMaybe (logNormalParams mean vol) mMuS
  x <- Rand.sample (Rand.Normal mu s)
  return  (exp x, LogNormal mean vol (Just (mu,s)))


applyFactor :: (IsRateModelFactor rmf, RateModelC m) => RateTag -> rmf -> m rmf
applyFactor tag factor = do
  (rates,src) <- get
  let ((newRate,newF),newSrc) = runState (rateModelFactorF factor) src
      newRates = rSet rates tag newRate
  put (newRates,newSrc)
  return newF



-- models, all based on factors

data BaseRateModel rmf = SingleFactorModel RateTag rmf |
                         ListModel [BaseRateModel rmf] |
                         SameModel RateType rmf |
                         GroupedModel RateType rmf deriving (Generic,ToJSON,FromJSON)


instance Show rmf=>Show (BaseRateModel rmf) where
  show (SingleFactorModel tag factor) = "Single Factor for " ++ show tag ++ "=>" ++ show factor
  show (ListModel models) = "List: " ++ show models
  show (SameModel rType factor) = "Same single-factor model for type=" ++ show rType ++"=>" ++ show factor
  show (GroupedModel rType factor) = "Same single-factor for type=" ++ show rType ++ "=>" ++ show factor

instance IsRateModelFactor rmf=>IsRateModel (BaseRateModel rmf) where
  rateModelF = baseRateModelF


baseRateModelF :: (IsRateModelFactor rmf, RateModelC m) => BaseRateModel rmf -> m (BaseRateModel rmf)
baseRateModelF s@(SingleFactorModel tag factor) = applyFactor tag factor >> return s
baseRateModelF (ListModel models) = ListModel <$> mapM rateModelF models
baseRateModelF (SameModel rType rmf) = do
  (rates,_) <- get
  let keys = filter (isRateType rType) (rKeys rates)
  newRMF <- foldM (flip applyFactor) rmf keys
  return $! SameModel rType newRMF
baseRateModelF (GroupedModel rType rmf) = do
  (rates,src) <- get
  let ((newRate,newF),newSrc) = runState (rateModelFactorF rmf) src
      keys = filter (isRateType rType) (rKeys rates)
  let newRates = foldl (\table key->(rSet table) key newRate) rates keys
  put (newRates,newSrc)
  return $! GroupedModel rType rmf -- why isn't this newF ??


{-
listModelF::RateModelC m=>[RateModel]->m RateModel
listModelF models = do
    newModels <- mapM rateModelF models
    return $ MkRateModel $ ListModel newModels


sameFactorModelF::RateModelC m=>RateType->RateModelFactor->m RateModel
sameFactorModelF rType fac = do
  (rates,_) <- get
  let keys = filter (isRateType rType) (rKeys rates)
  newF <- foldM (flip applyFactor) fac keys
  return $! MkRateModel $ SameFactorModel rType (MkRateModelFactor newF)

groupedFactorModelF::RateModelC m=>RateType->RateModelFactor->m RateModel
groupedFactorModelF rType fac = do
  (rates,src) <- get
  let ((newRate,newF),newSrc) = runState (rateFactorF fac) src
      keys = filter (isRateType rType) (rKeys rates)
  let newRates = foldl (\table key->(rSet table) key newRate) rates keys
  put (newRates,newSrc)
  return $! MkRateModel $ GroupedFactorModel rType fac

-}

{-
data SingleFactorModel rmf = SingleFactorModel { sfmTag::RateTag, sfmFactor::rmf } deriving (Generic)

instance TypeNamed SingleFactorModel

instance ToJSON SingleFactorModel where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3}

instance EnvFromJSON e RateModelFactor=>EnvFromJSON e SingleFactorModel where
  envParseJSON = genericEnvParseJSON defaultOptions { fieldLabelModifier = drop 3}




instance IsRateModel SingleFactorModel where
  rateModelF (SingleFactorModel tag factor) = factorModelF tag factor

data ListModel = ListModel  { lmModels::[RateModel] } deriving (Generic)

instance TypeNamed ListModel


instance ToJSON ListModel where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

instance HasParsers e RateModel=>EnvFromJSON e ListModel where
  envParseJSON = genericEnvParseJSON defaultOptions { fieldLabelModifier = drop 2 }



instance IsRateModel ListModel where
  rateModelF (ListModel models) = listModelF models

data SameFactorModel = SameFactorModel { sfType::RateType, sfFactor::RateModelFactor  } deriving (Generic)

instance TypeNamed SameFactorModel

instance ToJSON SameFactorModel where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

instance EnvFromJSON e RateModelFactor=>EnvFromJSON e SameFactorModel where
  envParseJSON = genericEnvParseJSON defaultOptions { fieldLabelModifier = drop 2 }


instance IsRateModel SameFactorModel where
  rateModelF (SameFactorModel rType factor) = sameFactorModelF rType factor

data GroupedFactorModel = GroupedFactorModel { gfType::RateType, gfFactor::RateModelFactor } deriving (Generic)

instance TypeNamed GroupedFactorModel

instance ToJSON GroupedFactorModel where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

instance EnvFromJSON e RateModelFactor=>EnvFromJSON e GroupedFactorModel where
  envParseJSON = genericEnvParseJSON defaultOptions { fieldLabelModifier = drop 2 }



instance IsRateModel GroupedFactorModel where
  rateModelF (GroupedFactorModel rType factor) = groupedFactorModelF rType factor

-}

testF::IO ()
testF = do
  src<-newPureMT
  let rt = defaultRateTable
      srm = SingleFactorModel (Return Stock) $ makeLogNormalFactor 0.005 0.002
      grm = SameModel IsReturn $ Normal 0 0.1
      mrm = ListModel [srm, grm]
  rt' <- eitherToIO $ applyModel (rt,src) srm
  rt'' <- eitherToIO $ applyModel (rt,src) grm
  rt''' <- eitherToIO $ applyModel (rt,src) mrm
  print rt
  print $ fst $ snd rt'
  print $ fst(snd rt'' )
  print $ (fst.snd) rt'''


{-
instance IsRateFactor LogNormalRateModelFactor where
  rateFactorF (LogNormalRateModelFactor mean vol mMu mS) = logNormalRateModelF mean vol mMu mS


data FixedRateModelFactor = FixedRateModelFactor { fRate::Rate } deriving (Generic,ToJSON,FromJSON)

instance TypeNamed FixedRateModelFactor

fixedRateModelF::RateFactorC m=>Rate->m (Rate,RateModelFactor)
fixedRateModelF x = return (x, MkRateModelFactor $ FixedRateModelFactor x)

instance IsRateFactor FixedRateModelFactor where
  rateFactorF (FixedRateModelFactor r) = fixedRateModelF r

data NormalRateModelFactor = NormalRateModelFactor {nMean::Double, nVol::Double } deriving (Show,Generic,ToJSON,FromJSON)

instance TypeNamed NormalRateModelFactor

normalRateModelF::RateFactorC m=>Double->Double->m (Rate,RateModelFactor)
normalRateModelF mean vol = do
  x <- sample (Normal mean vol)
  return (x,MkRateModelFactor $ NormalRateModelFactor mean vol)

instance IsRateFactor NormalRateModelFactor where
  rateFactorF (NormalRateModelFactor mean vol) = normalRateModelF mean vol


data LogNormalRateModelFactor = LogNormalRateModelFactor { lnMean::Double, lnVol::Double, lnMu::Maybe Double, lnS::Maybe Double } deriving (Generic)


-- custom instance so the output is simpler
instance ToJSON LogNormalRateModelFactor where
  toJSON (LogNormalRateModelFactor m v _ _) = object [ "lnMean" .= m, "lnVol" .= v]

instance FromJSON LogNormalRateModelFactor where
  parseJSON (Object v) = (\x y -> LogNormalRateModelFactor x y Nothing Nothing) <$> v .: "lnMean" <*> v .: "lnVol"
  parseJSON _          = fail "LogNormalRateModelFactor: non-object"

instance TypeNamed LogNormalRateModelFactor

-}

{-
data RateModelFactor where
  MkRateModelFactor::(TypeNamed a, IsRateFactor a, ToJSON a)=> a->RateModelFactor

instance JSON_Existential RateModelFactor where
  containedName (MkRateModelFactor a) = typeName a
  containedJSON (MkRateModelFactor a) = toJSON a

instance ToJSON RateModelFactor where
  toJSON = existentialToJSON

instance HasParsers e RateModelFactor => EnvFromJSON e RateModelFactor where
  envParseJSON = parseJSON_Existential

instance TypeNamed RateModelFactor where
  typeName = const "RateModelFactor"

instance IsRateFactor RateModelFactor where
  rateFactorF (MkRateModelFactor x) = rateFactorF x
-}
