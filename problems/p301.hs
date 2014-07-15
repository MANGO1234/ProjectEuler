import System.IO
import System.CPUTime
import Numeric

c :: Int -> (Int, Int)
c 1 = (0, 1)
c n = let (c0, c1) = c (n-1) in (c0+c1, c0)

answer :: Int
answer = sum $ map (\(a, b) -> a + b) $ map c [1..30]

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

showtime :: Integer -> String
showtime t = showFFloat (Just 3) (fromIntegral t / (10^12)) " seconds"

main :: IO ()
main = do
	t <- getCPUTime
	putStrLn $ show answer
	putStrLn $ show $ (fib !! 32)-1
	t' <- getCPUTime
	putStrLn $ "Time: " ++ showtime(t' - t)