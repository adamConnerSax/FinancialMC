{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module BenchMoneyOps
       (
         benchMoneyOpsIO  
       ) where

import Criterion
import Control.DeepSeq (NFData(rnf))



import FinancialMC.Core.MoneyValue
import qualified FinancialMC.Core.MoneyValueOps as MV
import qualified FinancialMC.Core.CValued as CV

d1 = 1.0 :: Double
d2 = 2.0 :: Double
d3 = 2.1 :: Double
usd1 = MoneyValue d1 USD
usd2 = MoneyValue d2 USD
eur1 = MoneyValue d2 EUR


eX = defaultExchangeRates

addOne x a = x + a
mvAddOne x a = MV.inFirst eX (+) x a
cvAddOne x a = CV.toMoneyValue USD eX $ CV.fromMoneyValue x CV.|+| CV.fromMoneyValue a

addTwo x y z = x + y + z
mvAddTwo x y z = let f = MV.inFirst eX (+) in f x (f y z)
cvAddTwo x y z = CV.toMoneyValue USD eX $ CV.fromMoneyValue x CV.|+| CV.fromMoneyValue y CV.|+| CV.fromMoneyValue z 

benchAdd1 =  bgroup "addOne" [ bench "doubles" $ nf (addOne d1) d2
                            , bench "MV_same" $ nf (mvAddOne usd1) usd2
                            , bench "MV_diff" $ nf (mvAddOne usd1) eur1                           
                            , bench "CV_same" $ nf (cvAddOne usd1) usd2
                            , bench "CV_diff" $ nf (cvAddOne usd1) eur1
                            ]


benchAdd2 =  bgroup "addTwo" [ bench "doubles" $ nf (addTwo d1 d2) d3
                            , bench "MV" $ nf (mvAddTwo usd1 usd2) eur1
                            , bench "CV" $ nf (cvAddTwo usd1 usd2) eur1
                            ]


sc1 = 3.1 :: Double
multScalar  x y = x*y
mvMultScalar mv sc = MV.multiply mv sc
cvMultScalar mv@(MoneyValue _ c) sc = CV.toMoneyValue c eX $ CV.fromMoneyValue mv CV.|*| sc

benchMult = bgroup "scalarMult" [ bench "doubles" $ nf (multScalar d1) sc1
                                , bench "MV" $ nf (mvMultScalar usd1) sc1
                                , bench "CV" $ nf (cvMultScalar usd1) sc1 ]


divide x y = x/y
mvDivide mv1 mv2 = MV.ratio eX mv1 mv2
cvDivide mv1 mv2 = CV.atERF eX $ (CV.fromMoneyValue mv1 CV.|/| CV.fromMoneyValue mv2)

benchDiv = bgroup "divOne" [ bench "doubles" $ nf (divide d1) d2
                           , bench "MV_same" $ nf (mvDivide usd1) usd2
                           , bench "MV_diff" $ nf (mvDivide usd1) eur1
                           , bench "CV_same" $ nf (cvDivide usd1) usd2
                           , bench "CV_diff" $ nf (cvDivide usd1) eur1
                           ]


arithmetic x y z = z * (max 0 (y - x)/y)
arithmeticCV x y z =
  let x' = CV.fromMoneyValue x
      y' = CV.fromMoneyValue y
  in CV.atERF eX $ z CV.|*| (CV.cvMax CV.scZero ((y' CV.|-| x') CV.|/| y')) 

benchArithmetic = bgroup "arithmetic"
                  [
                    bench "doubles" $ nf (arithmetic d1 d2) d3
                  , bench "CV" $ nf (arithmeticCV usd1 eur1) d1
                  ]


data SP = SP !CV.CVD !CV.CVD
instance NFData SP where
  rnf (SP x y) = rnf x `seq` rnf y `seq` rnf ()

doPaymentCV::Currency->CV.CVD->Double->SP->SP
doPaymentCV ccy pmt' rate (SP prin' paidSoFar') = 
  let interest' = rate CV.|*| prin'
      prinRemaining' = CV.cvMax (CV.mvZero ccy) (prin' CV.|+| interest' CV.|-| pmt')
      prinPaid' = prin' CV.|-| prinRemaining'
      amtPaid' = interest' CV.|+| prinPaid' CV.|+| paidSoFar'
  in SP prinRemaining' amtPaid' 


doPaymentCV2::ExchangeRateFunction->Currency->CV.CVD->Double->SP->SP
doPaymentCV2 e ccy pmt' rate (SP prin' paidSoFar') = 
  let pmtD =  CV.unwrap ccy e pmt'
      prinD = CV.unwrap ccy e prin'
      paidSoFarD = CV.unwrap ccy e paidSoFar'
      SD x y = doPayment pmtD rate (SD prinD paidSoFarD)
  in SP (CV.fromMoneyValue (MoneyValue x ccy)) (CV.fromMoneyValue (MoneyValue y ccy)) 


doPaymentCV3::ExchangeRateFunction->Currency->CV.CVD->Double->SP->SP
doPaymentCV3 e ccy pmt' rate (SP prin' paidSoFar') = 
  let pmtD =  CV.unwrap ccy e pmt'
      prinD = CV.unwrap ccy e prin'
      paidSoFarD = CV.unwrap ccy e paidSoFar'
      intD = rate * prinD
      pRemainingD = max 0 (prinD + intD - pmtD)
      pPaidD = prinD - pRemainingD
      aPaidD = intD + pPaidD + paidSoFarD
  in SP (CV.toCV pRemainingD ccy) (CV.toCV aPaidD ccy) 



doPaymentsCV::Currency->CV.CVD->Double->CV.CVD->Int->(CV.CVD,CV.CVD) 
doPaymentsCV ccy pmt rate prin n = let SP a b = foldl (\a _ -> doPaymentCV ccy pmt rate a) (SP prin (CV.mvZero ccy)) [1..n] in (a,b)
  
data SD = SD !Double !Double
instance NFData SD where
  rnf (SD x y) = rnf x `seq` rnf y `seq` ()

doPayment::Double->Double->SD->SD
doPayment pmt rate (SD prin paidSoFar) =
  let int = rate * prin
      pRemaining = max 0 (prin + int - pmt)
      pPaid = prin - pRemaining
      aPaid = int + pPaid + paidSoFar
  in SD pRemaining aPaid




doPayments::ExchangeRateFunction->Currency->CV.CVD->Double->CV.CVD->Int->(CV.CVD,CV.CVD)
doPayments e ccy pmt' rate prin' n = 
  let pmt = CV.unwrap ccy e pmt'
      prin  = CV.unwrap ccy e prin'
      SD pRemaining aPaid = foldl (\a _ -> doPayment pmt rate a) (SD prin 0) [1..n]
  in (CV.fromMoneyValue (MoneyValue pRemaining ccy),CV.fromMoneyValue (MoneyValue aPaid ccy))

pmtCV = CV.fromMoneyValue (MoneyValue 100 USD)
prinCV = CV.fromMoneyValue (MoneyValue 10000 USD)

benchMortgage = bgroup "mortgage"
                [
                  bench "doPayment (doubles)" $ nf (doPayment 100 0.05) (SD 10000 0)
                , bench "doPayment (CV)" $ nf (doPaymentCV USD pmtCV 0.05) (SP prinCV (CV.mvZero USD))
                , bench "doPayment (hybrid)" $ nf (doPaymentCV2 eX USD pmtCV 0.05) (SP prinCV (CV.mvZero USD))
                , bench "doPayment (hybrid2)" $ nf (doPaymentCV3 eX USD pmtCV 0.05) (SP prinCV (CV.mvZero USD))
                , bench "doPayments (doubles)" $ nf (fst . doPayments eX USD pmtCV 0.05 prinCV) 12
                , bench "doPayments (CV)" $ nf (fst . doPaymentsCV USD pmtCV 0.05 prinCV) 12
                ]

benchMoneyOpsIO:: IO Benchmark
benchMoneyOpsIO =  return $ bgroup "MoneyOps"
                 [
                   benchAdd1
                 , benchAdd2
                 , benchMult
                 , benchDiv
                 , benchArithmetic
                 , benchMortgage
                 ]
