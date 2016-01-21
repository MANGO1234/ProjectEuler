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
merge xx@(x:xs) yy@(y:ys) = if x <= y then x:merge xs yy else y:merge xx ys

kmerge :: (Ord a) => [[a]] -> [a]
kmerge = foldt merge

-- merge list of sorted list where the lists in the infinite list are sorted by first element, allows infinite lists
kmergei :: (Ord a) => [[a]] -> [a]
kmergei = kmergeiBySort compare

kmergeiBySort :: (a -> a -> Ordering) -> [[a]] -> [a]
kmergeiBySort comp = kmergeii
    where kmergeii [] = []
          kmergeii [x] = x
          kmergeii ([]:ls) = kmergeii ls
          kmergeii ((x:xs):ls) = x:kmergeii (insert xs ls)
          insert [] ls = ls
          insert as [] = [as]
          insert as ([]:ls) = insert as ls
          insert as@(a:_) ll@(xx@(x:xs):ls) = case comp a x of
              LT -> as:ll
              EQ -> as:ll
              GT -> xx:insert as ls

cross :: (a -> b -> c) -> [a] -> [b] -> [[c]]
cross f as bs = fmap (\a -> fmap (f a) bs) as

unfoldl :: (a -> Maybe (b, a)) -> a -> [b]
unfoldl f = helper
    where helper x = case f x of
            Nothing -> []
            Just (n, x) -> n:helper x

groupToSingle :: (Eq a) => [a] -> [a]
groupToSingle [] = []
groupToSingle xs = head xs:helper (tail xs) (head xs)
    where
        helper [] _ = []
        helper (x:xs) last = if x == last then helper xs last else x:helper xs x
