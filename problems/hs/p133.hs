module Main where

import Data.List
import Data.Maybe
import PrimeUtil
import NumberUtil

-- -- from forum
p = extractPrimes . eratosthenesSieve $ 100000
k = filter (\i -> powmod 10 (10^16) (9*i) /= 1) p

-- -- original brute force
-- removeFactor n x = let (q, r) = x `quotRem` n in if r == 0 then removeFactor n q else x
-- p = filter (/=5) . tail . extractPrimes . eratosthenesSieve $ 100000
-- g n = (1+) . fromJust . elemIndex 0 $ iterate (\i -> (i * 10 + 1) `mod` n) 1
-- k = map fst . filter (\(_, i) -> (removeFactor 2 . removeFactor 5 $ i) /= 1) $ zip p (map g p)

main :: IO ()
main = print $ sum k
