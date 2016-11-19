import System.IO
import System.CPUTime
import Data.Ratio
import Data.List
import Data.Ord
import Numeric

ops :: [Rational -> Rational -> Rational]
ops = [(\a b -> a + b)
      ,(\a b -> a - b)
      ,(\b a -> a - b) -- since - is not commutative, use this to avoid missing solutions, same with /
      ,(\a b -> a * b)
      ,(\a b -> if (b == 0) then -10000000000000 / 71234417 else a / b)
      ,(\b a -> if (b == 0) then -10000000000000 / 71234417 else a / b)] -- use random fraction that will be discarded later

possible :: [Rational] -> [Integer]
possible digits = sort . nub . map numerator $ filter (\x -> x > 0 && denominator x == 1) nums
    where nums = [h d $ g c $ f a b | [a, b, c, d] <- permutations digits, f <- ops, g <- ops, h <- ops] -- generate all numbers

process :: Integer -> [Integer] -> Integer
process n (x:xs) = if (x == n + 1) then process (n + 1) xs else n

answer = maximumBy (comparing snd) $ map (\x -> (map numerator x, process 0 $ possible x)) $ filter (\x->length x == 4) (subsequences [1..9])

showtime :: Integer -> String
showtime t = showFFloat (Just 3) (fromIntegral t / (10^12)) " seconds"

main :: IO ()
main = do 
	t <- getCPUTime
	putStrLn . show $ answer
	t' <- getCPUTime
	putStrLn $ "Time: " ++ showtime(t' - t)