module Time (time) where

import Text.Printf
import System.CPUTime

time :: IO b -> IO b
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
