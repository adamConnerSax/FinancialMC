module BenchUtils where

import Criterion

import Control.Monad (liftM)

bgroupM::String->IO [Benchmark] -> IO Benchmark
bgroupM = liftM . bgroup 
