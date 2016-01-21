module NumberUtil where

import Data.List
import Data.Maybe

-- optimize?
class Integral a => HasDigit a where
    numberOfDigits :: a -> Int

p10 = iterate (*10) 1

instance HasDigit Int where
    numberOfDigits n = let k = toInteger n in if n == 0 then 1 else fromJust . findIndex (>k) $ p10

instance HasDigit Integer where
    numberOfDigits n = let k = toInteger n in if n == 0 then 1 else fromJust . findIndex (>k) $ p10

-- repurposed from prelude
powmod :: Int -> Int -> Int -> Int
powmod n x m
        | x < 0    = error "Negative exponent"
        | x == 0   = 1
        | otherwise = f n x
    where
          f x y | even y    = f (mod (x * x) m) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (mod (x * x) m) ((y - 1) `quot` 2) x
          g x y z | even y = g (mod (x * x) m) (y `quot` 2) z
                  | y == 1 = mod (x * z) m
                  | otherwise = g (mod (x * x) m) ((y - 1) `quot` 2) (mod (x * z) m)

linearDiophantine :: Int -> Int -> Int -> Maybe (Int, Int)
linearDiophantine x' y' z = if z `mod` gcd == 0 then Just ans else Nothing
    where
        x = abs x'
        y = abs y'
        gcdList :: Int -> Int -> [Int] -> (Int, [Int])
        gcdList x y acc = let (q, r) = quotRem x y in if r == 0 then (y, acc) else gcdList y r (-q:acc)
        (gcd, qs) = gcdList x y []
        (a', b') = foldl' (\(a, b) n -> (b, a + b * n)) (1, head qs) (tail qs)
        k = z `quot` gcd
        a = if x == x' then a' else -a'
        b = if y == y' then b' else -b'
        ans = (k * a, k * b)
