import System.IO
import System.CPUTime
import Numeric

m :: Integer -> Integer -> Integer
m r c = if c >= (r+1) then r+1 else m' (r+1-c) c c

m' :: Integer -> Integer -> Integer -> Integer
m' d c a
	| d == 0 = a
	| otherwise =
		let (x, y) = divMod (a - 1) (c - 2)
		    a' = if y == 0 then x*c else x*c+2+y
		in m' (d - 1) c a'

showtime :: Integer -> String
showtime t = showFFloat (Just 3) (fromIntegral t / (10^12)) " seconds"

main :: IO ()
main = do
	t <- getCPUTime
	putStrLn . show . sum . map (m 30) $ [3..40]
	t' <- getCPUTime
	putStrLn $ "Time: " ++ showtime(t' - t)