{-# LANGUAGE GADTs,BangPatterns #-}
module PrimeUtil where
    -- eratosthenesSieveLowerMemory
    -- , sieve
    -- , updateVector
    -- , sieveU
    -- , updateVectorU
    -- , eratosthenesSieve
    -- , totientSieve
    -- , uniquePrimeFactorsSieve
    -- , extractPrimes
-- ) where

import qualified Data.Array.ST as AS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.Primitive (PrimState)
import Data.Array.Unboxed (UArray)
import Control.Monad (liftM, when)
import Control.Monad.ST (ST, runST)
import System.IO.Unsafe
import Util
import Debug.Trace

type Sieve = U.Vector Bool
type SieveL = UArray Int Bool

eratosthenesSieveLowerMemory :: Int -> SieveL
eratosthenesSieveLowerMemory n = AS.runSTUArray (
 do arr <- AS.newArray (1, n) False :: ST s (AS.STUArray s Int Bool)
    AS.writeArray arr 1 True
    let write start interval = helper start where helper i = when (i < n) (AS.writeArray arr i True >> helper (i + interval))
    mapM_ (\i -> let !k = if i == 2 then i else 2 * i in whenM (fmap not (AS.readArray arr i)) (write (i * i) k)) [2..floor . sqrt . fromIntegral $ n]
    return arr
    )

sieve :: VM.MVector (PrimState (ST s)) e -> [Int] -> (e -> Bool) -> (Int -> ST s ()) -> ST s ()
sieve arr indexes p update = mapM_ (\i -> whenM (fmap p (VM.unsafeRead arr i)) (update i)) indexes

updateVector :: VM.MVector (PrimState (ST s)) e -> Int -> Int -> Int -> (Int -> e -> e) -> ST s ()
updateVector arr start interval end update = helper start
    where helper i = when (i <= end) (fmap (update i) (VM.unsafeRead arr i) >>= VM.unsafeWrite arr i >> helper (i + interval))

updateVector2 :: VM.MVector (PrimState (ST s)) e -> Int -> Int -> Int -> (e -> e) -> ST s ()
updateVector2 arr start interval end update = helper start
    where helper i = when (i <= end) (fmap update (VM.unsafeRead arr i) >>= VM.unsafeWrite arr i >> helper (i + interval))

sieveU :: (UM.Unbox e) => UM.MVector (PrimState (ST s)) e -> [Int] -> (e -> Bool) -> (Int -> ST s ()) -> ST s ()
sieveU arr indexes p update = mapM_ (\i -> whenM (fmap p (UM.unsafeRead arr i)) (update i)) indexes

updateVectorU :: (UM.Unbox e) => UM.MVector (PrimState (ST s)) e -> Int -> Int -> Int -> (Int -> e -> e) -> ST s ()
updateVectorU arr start interval end update = helper start
    where helper i = when (i <= end) (fmap (update i) (UM.unsafeRead arr i) >>= UM.unsafeWrite arr i >> helper (i + interval))

eratosthenesSieve :: Int -> Sieve
eratosthenesSieve n = runST (
 do arr <- UM.replicate (n + 1) True :: ST s (UM.MVector (PrimState (ST s)) Bool)
    UM.write arr 0 False
    UM.write arr 1 False
    let update i = let !k = if i == 2 then i else 2 * i in updateVectorU arr (i*i) k n (const (const False))
    sieveU arr [2..floor . sqrt . fromIntegral $ n] id update
    U.unsafeFreeze arr
    )

totientSieve :: Int -> U.Vector Int
totientSieve n = runST (
 do arr <- UM.replicate (n + 1) 0 :: ST s (UM.MVector (PrimState (ST s)) Int)
    UM.write arr 1 1
    sieveU arr [2..n] (==0) (\i -> updateVectorU arr i i n (\m x -> if x == 0 then m `div` i * (i-1) else x `div` i * (i-1)))
    U.unsafeFreeze arr
    )

uniquePrimeFactorsSieve :: Int -> V.Vector [Int]
uniquePrimeFactorsSieve n = runST (
 do arr <- VM.replicate (n + 1) [] :: ST s (VM.MVector (PrimState (ST s)) [Int])
    sieve arr [2..n] null (\i -> updateVector2 arr i i n (i:))
    V.unsafeFreeze arr
    )

primeFactorizationSieve :: Int -> V.Vector [Int]
primeFactorizationSieve n = runST (
 do arr <- VM.replicate (n + 1) [] :: ST s (VM.MVector (PrimState (ST s)) [Int])
    let consWhile i = mapM_ (\k -> updateVector2 arr k k n (i:)) . takeWhile (<=n) $ iterate (*i) i
    sieve arr [2..n] null consWhile
    V.unsafeFreeze arr
    )

extractPrimes :: Sieve -> [Int]
extractPrimes s = if l >= 2 then 2:helper 3 else []
    where l = U.length s
          helper i = if i < l then if s U.! i then i:helper (i+2) else helper (i+2) else []
