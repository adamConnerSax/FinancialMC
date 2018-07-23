{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module FinancialMC.Parsers.XML.ParseTax 
       (
         TaxStructure(..)
       , emptyTaxStructure
       , loadTaxDataFromFile
       , loadTaxDataFromString
       ) where

import           FinancialMC.Core.MoneyValue (MoneyValue)
-- import           FinancialMC.Core.MoneyValueOps (zero)
import           FinancialMC.Core.Tax (FilingStatus(..),TaxBrackets,buildTaxBracketsFromTops,TaxBracket(Bracket,TopBracket),makeTaxBrackets,
                                      FedCapitalGains(..),CapGainBand(..))

import           FinancialMC.Parsers.XML.Utilities (atTag,readAttrValue,buildOpts,XmlParseInfos,runFMCX)
import           FinancialMC.Parsers.Configuration (MapByFS,makeFSMap,FederalTaxStructure(..),StateTaxStructure(..),CityTaxStructure(..),TaxStructure(..),
                                                    emptyTaxStructure,mergeTaxStructures)

import           Text.XML.HXT.Core (listA,returnA,(>>>),ArrowChoice,ArrowXml,withRemoveWS,yes,readString,IOSLA,XIOState,
                                   XmlTree,getChildren,getElemName,localPart,getAttrValue)
import           Data.List (sortBy)
import           Data.Ord (comparing)
-- import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Safe 
import           Control.Monad.Trans (MonadTrans,lift)
import           Control.Monad.State.Strict (MonadState,StateT,get,put,execState)

loadTaxDataFromFile::Maybe FilePath->FilePath->StateT TaxStructure IO ()
loadTaxDataFromFile mSchemaDir file =
  lift (readFile file) >>= loadTaxDataFromString mSchemaDir

  
loadTaxDataFromString::Maybe FilePath->String->StateT TaxStructure IO ()
loadTaxDataFromString mSchemaDir content = do
  let opts = buildOpts mSchemaDir [withRemoveWS yes] "Tax.rng"
  let xml = readString opts content
  loadTaxData' xml

loadTaxData'::(MonadTrans t, MonadState TaxStructure (t IO))=>IOSLA (XIOState XmlParseInfos) XmlTree XmlTree -> t IO ()
loadTaxData' xml = do  
  ts <- get
  result <- lift $ runFMCX (xml >>> parseTaxXml ts)
  put $ head result -- returnA always returns a list even if only 1 result


parseBracket::ArrowXml a=>a XmlTree TaxBracket
parseBracket = proc l -> do 
    bottom <- readAttrValue "bottom" -< l
    top    <- readAttrValue "top"    -< l
    rt   <- readAttrValue "rate"   -< l
    returnA -< Bracket bottom top (rt/100) 

parseTopBracket::ArrowXml a=>a XmlTree TaxBracket
parseTopBracket = proc l -> do 
  bottom <- readAttrValue "bottom" -< l
  rt   <- readAttrValue "rate"   -< l
  returnA -< TopBracket bottom (rt/100)

parseFullBracket::(ArrowXml a,ArrowChoice a)=>a XmlTree TaxBracket
parseFullBracket = getChildren >>> 
  proc l -> do
    bktType <- getElemName -< l
    bkt <- case localPart bktType of
      "Bracket" -> parseBracket -< l
      "TopBracket" -> parseTopBracket -< l
    returnA -< bkt
    
parseFullBrackets::(ArrowXml a,ArrowChoice a)=>a XmlTree TaxBrackets
parseFullBrackets = atTag "Brackets" >>> 
                    proc l -> do
                      tbs <- listA parseFullBracket -< l
                      returnA -< makeTaxBrackets tbs

parseBracketTops::ArrowXml a=>a XmlTree (Double,Double)
parseBracketTops = atTag "BracketTop" >>>
  proc l -> do
    top <- readAttrValue "top" -< l
    rt <- readAttrValue "rate" -< l
    returnA -< (top,rt/100)

parseSimpleBrackets::ArrowXml a=>a XmlTree TaxBrackets
parseSimpleBrackets = atTag "SimpleBrackets" >>>
  proc l -> do
    topRt <- readAttrValue "top_rate" -< l
    currency <- readAttrValue "currency" -< l
    bktTops <- listA parseBracketTops -< l
    returnA -< buildTaxBracketsFromTops currency (sortBy (flip (comparing fst)) bktTops) (topRt/100)
    
parseIncomeTaxStructure::ArrowXml a=>a XmlTree (FilingStatus,TaxBrackets)
parseIncomeTaxStructure = atTag "IncomeTaxStructure" >>>
  proc l -> do
    fStatus <- readAttrValue "status" -< l
    brackets <- parseSimpleBrackets -<< l
    returnA -< (fStatus,brackets)
    
parsePayrollTaxStructure::(ArrowXml a,ArrowChoice a)=>a XmlTree TaxBrackets   
parsePayrollTaxStructure = atTag "PayrollTaxStructure" >>> parseFullBrackets


parseEstateTaxStructure::(ArrowXml a, ArrowChoice a)=>a XmlTree TaxBrackets   
parseEstateTaxStructure = atTag "EstateTaxStructure" >>> parseFullBrackets

--This only works this way bec FedCapitalGains is just rate bands.  If that changes...
parseFedCapitalGains::(ArrowXml a)=>a XmlTree FedCapitalGains
parseFedCapitalGains = atTag "CapitalGains" >>> parseRateBands

parseRateBands::ArrowXml a=>a XmlTree FedCapitalGains
parseRateBands = atTag "RateBands" >>>
  proc l -> do
    topRt <- readAttrValue "topRate" -< l
    bands <- listA parseRateBand -< l
    returnA -< FedCapitalGains (topRt/100) bands

parseRateBand::ArrowXml a=>a XmlTree CapGainBand
parseRateBand = atTag "Band" >>>
  proc l -> do
    top <- readAttrValue "top" -< l
    rt <- readAttrValue "rate" -< l
    returnA -< CapGainBand (top/100) (rt/100)

parseMedicareSurtax::ArrowXml a=>a XmlTree (Double, MapByFS MoneyValue)
parseMedicareSurtax = atTag "MedicareSurtax" >>>
  proc l -> do
    nirt <- readAttrValue "rate" -< l
    thresholds <- listA parseThreshold -< l
    returnA -< (nirt/100, makeFSMap $ M.fromList thresholds)

parseThreshold::ArrowXml a=>a XmlTree (FilingStatus,MoneyValue)
parseThreshold = atTag "Threshold" >>>
  proc l -> do
    fStatus <- readAttrValue "status" -< l
    amount <- readAttrValue "amount" -< l
    returnA -< (fStatus,amount)

parseStandardDeductions :: ArrowXml a => a XmlTree (MapByFS MoneyValue)
parseStandardDeductions = atTag "StandardDeduction" >>>
  proc l -> do
    deductions <- listA parseStandardDeduction -< l
    returnA -< makeFSMap $ M.fromList deductions

parseStandardDeduction :: ArrowXml a => a XmlTree (FilingStatus, MoneyValue)
parseStandardDeduction = atTag "SD" >>>
  proc l -> do
    fStatus <- readAttrValue "status" -< l
    ded <- readAttrValue "deduction" -< l
    returnA -< (fStatus, ded)

parseSALTCap :: ArrowXml a => a XmlTree MoneyValue
parseSALTCap = atTag "SALTCap" >>>
  proc l -> do
    sCap <- readAttrValue "SALT_cap" -< l 
    returnA -< sCap

parseFederalTaxStructure::(ArrowXml a,ArrowChoice a)=>a XmlTree (String,FederalTaxStructure)
parseFederalTaxStructure = atTag "FederalTaxStructure" >>>
  proc l -> do
    name <- getAttrValue "name" -< l
    incomeTaxL <- listA parseIncomeTaxStructure -< l
    payrollTax <- parsePayrollTaxStructure -< l
    estateTax <- parseEstateTaxStructure -< l
    capitalGains <- parseFedCapitalGains -< l
    medSurtax <- parseMedicareSurtax -< l
    standardDeductionsByFS <- parseStandardDeductions -< l
    saltCapL <- listA parseSALTCap -< l
    let saltCapM = Safe.headMay saltCapL -- this is not an ideal solution to > 1
    returnA -< (name,FederalTaxStructure (makeFSMap $ M.fromList incomeTaxL) payrollTax estateTax capitalGains medSurtax standardDeductionsByFS saltCapM)

{-
parseStateCapitalGains::ArrowXml a=>a XmlTree Double
parseStateCapitalGains = atTag "CapitalGains" >>>
  proc l -> do
    rt <- readAttrValue "rate" -< l
    returnA -< rt/100
-}

parseStateTaxStructure :: ArrowXml a => a XmlTree (String, String, StateTaxStructure)
parseStateTaxStructure = atTag "StateTaxStructure" >>>
  proc l -> do
    state <- getAttrValue "state" -< l
    name <- getAttrValue "name" -< l
    incomeTaxL <- listA parseIncomeTaxStructure -< l
    standardDeductionsByFS <- parseStandardDeductions -< l
    returnA -< (state,name, StateTaxStructure (makeFSMap $ M.fromList incomeTaxL) standardDeductionsByFS)

parseCityTaxStructure::ArrowXml a=>a XmlTree (String,String,CityTaxStructure)
parseCityTaxStructure = atTag "CityTaxStructure" >>>
  proc l -> do
    city <- getAttrValue "city" -< l
    name <- getAttrValue "name" -< l
    incomeTaxL <- listA parseIncomeTaxStructure -< l
    returnA -< (city,name,CityTaxStructure (makeFSMap $ M.fromList incomeTaxL))

f (a,b,c) = (a ++ ": " ++ b,c)
parseTaxStructures::(ArrowXml a,ArrowChoice a)=>a XmlTree TaxStructure
parseTaxStructures = atTag "TaxStructures" >>>
  proc l -> do
    fedL <- listA parseFederalTaxStructure -< l
    stateL <- listA parseStateTaxStructure -< l
    cityL <- listA parseCityTaxStructure -< l
    returnA -< TaxStructure (M.fromList fedL) (M.fromList $ (f <$> stateL)) (M.fromList $ (f <$> cityL))


parseTaxXml::(ArrowXml a,ArrowChoice a)=>TaxStructure->a XmlTree TaxStructure
parseTaxXml ts =
  proc l -> do
    ts' <- parseTaxStructures -< l
    returnA -< execState (mergeTaxStructures ts') ts 

