import System.IO
import System.CPUTime
import Numeric

e :: Double
e = exp 1

maxP :: Double -> Int
maxP x = let n' = floor(x / e)
             n = (fromIntegral n')::Double
             a = n * log (x / n)
             b = (n + 1) * log (x / (n + 1))
	in if a >= b then n' else n'+1

maxP2 :: Double -> Int
maxP2 x = round (x / e) -- this works too, at least for this question

div2or5 :: Int -> Bool
div2or5 x
	| x == 1 = True
	| x `mod` 2 == 0 = div2or5(x `div` 2)
	| x `mod` 5 == 0 = div2or5(x `div` 5)
	| otherwise = False

terminate :: Int -> Bool
terminate x = let n = maxP (fromIntegral x)
	in div2or5(n `div` (gcd x n))

showtime :: Integer -> String
showtime t = showFFloat (Just 3) (fromIntegral t / (10^12)) " seconds"

main :: IO ()
main = do
	t <- getCPUTime
	putStrLn . show . sum . map (\x -> if terminate x then -x else x) $ [5..10000]
	t' <- getCPUTime
	putStrLn $ "Time: " ++ showtime(t' - t)