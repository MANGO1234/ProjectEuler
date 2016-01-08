module Util where

import Data.List
import Control.Monad (liftM, when)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p a = p >>= (`when` a)

pairs :: [a] -> [[a]]
pairs [] = []
pairs [x] = [[x]]
pairs (x:y:xs) = [x,y]:pairs xs

foldt :: (a -> a -> a) -> [a] -> a
foldt _ [] = undefined
foldt _ [a] = a
foldt f xs = foldt f (fmap (foldl1 f) (pairs xs))

merge :: (Ord a) => [a] -> [a] -> [a]
merge xx [] = xx
merge [] yy = yy
merge xx@(x:xs) yy@(y:ys) = if x < y then x:merge xs yy else y:merge xx ys

kmerge :: (Ord a) => [[a]] -> [a]
kmerge = foldt merge
