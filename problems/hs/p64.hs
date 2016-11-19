module Main where

import Util
import Data.List
import Data.Maybe

g :: Int -> [(Int, Int)]
g n = let k = floor . sqrt . fromIntegral $ n in
    unfoldl (\(a, b) ->
    let (q, r) = quotRem (b + k) a
        t = k - r
        ans = ((n - t ^ 2) `div` a, t) in
        if a == 0 then Nothing else Just (ans, ans)
    ) (1, 0)

main :: IO()
main = let xs = map (drop 1 . g) [2..10000]
           ls = map (\xx -> if null xx then 0 else (1+) . fromJust $ elemIndex (head xx) (tail xx)) xs
       in print . length . filter odd $ ls
