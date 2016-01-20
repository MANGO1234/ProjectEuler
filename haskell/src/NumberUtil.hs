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
