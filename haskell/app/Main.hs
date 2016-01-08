module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Criterion.Main
import Debug.Trace
import PrimeUtil
import Time

main :: IO()
main = do
    print $ extractPrimes $ eratosthenesSieve 100
    let k = eratosthenesSieve 10000000
    let l = length $ extractPrimes k
    defaultMain [
        bgroup "fib" [
                    bench "0" $ nf (\n -> eratosthenesSieve n U.! 1) 100
                    , bench "1" $ nf (\n -> extractPrimes n) k
                    -- , bench "1" $ nf (\n -> primeFactorizationSieve n V.! 1000000) 1000000
                    -- , bench "1" $ nf (\n -> uniquePrimeFactorsSieve n V.! 100000) 100000
                    ]
        ]
