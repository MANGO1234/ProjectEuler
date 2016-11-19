module Main where

import PrimeUtil
import NumberUtil

--64 bit Int needed
ps = drop 2 . extractPrimes . eratosthenesSieve $ 1000100
pp = takeWhile (\(p1, p2) -> p1 <= 1000000) $ zip ps (tail ps)
x = map (\(p1, p2) -> let {pow = 10^numberOfDigits p1; Just (a, b) = linearDiophantine p2 (-pow) p1} in p2 * (a `mod` pow)) pp

main :: IO()
main = print $ sum x
