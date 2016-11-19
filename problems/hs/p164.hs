module Main where

import qualified Data.Vector.Unboxed as U
import PrimeUtil

-- can probably be much faster without recalculating the totient chain always
s = totientSieve 40000000
t = sum . map (toInteger . head) . filter (\l -> length l == 24) . map (takeWhile (>1) . iterate (s U.!)) . filter (\i -> s U.! i == i - 1) $ [2..40000000]

main :: IO()
main = print t
