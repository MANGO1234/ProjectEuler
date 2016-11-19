module Main where

import Data.List
import Data.Maybe
import PrimeUtil
import NumberUtil

-- -- from forum
p = extractPrimes . eratosthenesSieve $ 1000000
k = filter (\i -> powmod 10 (10^9) (9*i) == 1) p

-- -- original brute force
-- p = filter (/=5) . tail . extractPrimes . eratosthenesSieve $ 1000000
-- g n = (1+) . fromJust . elemIndex 0 $ iterate (\i -> (i * 10 + 1) `mod` n) 1
-- k = map fst . filter (\(_, i) -> 1000000000 `mod` i == 0) $ zip p (map g p)

main :: IO ()
main = print $ sum . take 40 $ k
