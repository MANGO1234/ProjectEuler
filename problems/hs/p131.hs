module Main where

import PrimeUtil
import Data.List
import Util
import qualified Data.Vector.Unboxed as U

p = eratosthenesSieve 1000000
g = filter (\i -> p U.! i) . takeWhile (<1000000) . groupToSingle . kmerge . map k . tails $ map (^3) [1..1000]
    where
        k [] = []
        k [x] = []
        k (x:xs) = map (\i -> i-x) xs

-- forum: ah right a^3-b^3 is composite unless a = b +1
g2 = filter (\i -> p U.! i) . takeWhile (<1000000) . map (\(a,b) -> b - a) $ zip xs (tail xs)
    where xs = map (^3) [1..]

main :: IO ()
main = print $ (length g, length g2)
