module Main where

import PrimeUtil

primes = extractPrimes $ eratosthenesSieve 1000000
triangular n = n * (n + 1) `div` 2
a = zipWith t primes (tail primes)
    where
        t p1 p2 = p1 * (triangular (p2 * p2 `div` p1) - triangular p1) + p2 * (triangular (p2 - 1) - triangular (p1 * p1 `div` p2)) - 2 * p1 * p2

main :: IO ()
main = print $ sum a
