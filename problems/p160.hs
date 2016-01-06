{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.List
import Data.Functor.Identity

modulus = 100000::Word

type ModWord = Identity Word

instance Num ModWord where
  (+) (Identity a) (Identity b) = Identity ((a + b) `mod` modulus)
  (*) (Identity a) (Identity b) = Identity ((a * b) `mod` modulus)
  abs = id
  negate (Identity a) = let x = a `mod` modulus in if x == 0 then Identity 0 else Identity (modulus - x)
  signum _ = Identity 1
  fromInteger a = Identity (fromInteger a `mod` modulus)

fromWord :: Word -> ModWord
fromWord = Identity

toWord :: ModWord -> Word
toWord = runIdentity

p2 = iterate (*2) 1
p5 = iterate (*5) 1
pa = filter (\x -> 1 <= x && x <= 1000000000000) [x*y | x <- limit p2, y <- limit p5] where limit = takeWhile (<=1000000000000)

ls = scanl' h (fromWord 1) [1..modulus - 1] where h a b = if (b `mod` 5 == 0) || (b `mod` 2 == 0) then a else a * fromWord b
-- from forum comments, but the product of all number coprimes to modulus = 1
f n = ls !! (n `mod` 100000)

numOfFactor n p = sum . takeWhile (>=1) . drop 1 $ iterate (`div` p) n

main :: IO ()
main = print $ product (map (f . (1000000000000 `div`)) pa) * fromWord 2 ^ (numOfFactor 1000000000000 2 - numOfFactor 1000000000000 5)
