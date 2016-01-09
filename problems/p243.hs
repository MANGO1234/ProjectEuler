module Main where

import Util
import PrimeUtil
import Data.Ratio

ps = map toInteger . extractPrimes . eratosthenesSieve $ 100
pz = scanl (\(a, b, c) p -> (b*a, p, c * ((b - 1) % b))) (1, head ps, 1 % 1) (tail ps)
s = kmergeiBySort (\(_,a) (_,b) -> compare a b) $ map (\(pro, upto, r) -> map (\i -> (i* pro, r * ((i * pro) % (i * pro - 1)))) [1..upto]) (tail pz)
t = minimum . map fst . filter (\(_, r ) -> r < 15499 % 94744) $ s

main :: IO()
main = print t
