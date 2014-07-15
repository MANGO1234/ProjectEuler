import System.IO
import System.CPUTime
import Numeric

-- (accumulated differce, max difference, min difference, power, n)
type Range = (Integer, Integer, Integer, Int, Int)
sel1 (x,_,_,_,_) = x
sel2 (_,x,_,_,_) = x
sel3 (_,_,x,_,_) = x
sel4 (_,_,_,x,_) = x

pow10 :: [Integer]
pow10 = iterate (*10) 1

-- d is digit, p is power, cache[d][p] holds the Range for 0 to 10^p-1
cache :: [[Range]]
cache = map (\d -> iterate (gen d) (-1, -1, -1, 0, 0)) [0..]

next :: Int -> Range -> Range -> Range
next d (acc0, max0, min0, p, 0) (acc, _, _, _, i)
	| i + 1 == d = (max', max', acc, p, i + 1)
	| otherwise  = (acc + acc0, acc + max0, acc + min0, p, i + 1)
	--where max' = acc + if p == 0 then 0 else (toInteger p) * pow10 !! (p - 1)
	where max' = acc + sel1(cache !! d !! p) + pow10 !! p

gen :: Int -> Range -> Range
gen d range0 = (acc, max', min', pow, 0)
	where
		xs = take 10 $ iterate (next d range0) range0
		acc = last $ map sel1 xs
		max' = maximum $ map sel2 xs
		min' = minimum $ map sel3 xs
		pow = sel4 range0 + 1


snext :: Int -> Int -> Range -> Range
snext d dCount (acc, _, _, p, i)
	| i + 1 == d = (max1, max1, min1, p, i + 1)
	| dCount > 0 = (max2, max2, min2, p, i + 1)
	| otherwise  = (acc + nacc, acc + nmax, acc + nmin, p, i + 1)
	where
		range0 = cache !! d !! p
		(nacc, nmax, nmin, _, _) = range0
		max1   = acc + nacc + (toInteger (dCount + 1)) * pow10 !! p
		min1   = if p == 0 then max1 else acc
		max2   = acc + nacc + (toInteger dCount      ) * pow10 !! p
		min2   = if p == 0 then max2 else acc

find' :: Int -> Int -> Range -> [Integer] -> [Integer]
find' d dCount (acc, _, _, p, n) xs
	| p   == -1 = [toInteger n]
	| n+1 == 10 = xs
	| otherwise = let
		newRange = snext d dCount (acc, 0, 0, p, n)
		(_, nmax, nmin, _, nn) = newRange
		ndCount = if nn == d then dCount + 1 else dCount
		subList = find' d ndCount (acc, 0, 0, p - 1, if (p == 0) then nn else -1) []
		nxs     = xs ++ map (\x -> if p == 0 then x else x + (toInteger nn) * pow10 !! p) subList
		in if nmax >= -1 && nmin <= -1
			then find' d dCount newRange nxs
			else find' d dCount newRange xs

-- functional of above, without the need for iteration
find'' :: Int -> Int -> Range -> [Integer]
find'' d dCount (acc, _, _, p, n)
	| p == -1   = [toInteger n]
	| otherwise = concat $ map getSubSolutions validRanges
		where
			ranges = iterate (snext d dCount) (acc, 0, 0, p, -1)
			validRanges = filter containsSolution $ take 10 $ zip (tail ranges) ranges
			containsSolution = \(x, _) -> sel2 x >= -1 && sel3 x <= -1
			getSubSolutions ((_, _, _, _, nn), (oacc, _, _, _, _)) = xs where
				ndCount = if nn == d then dCount + 1 else dCount
				subList = find'' d ndCount (oacc, 0, 0, p - 1, nn)
				xs      = map (\x -> if p == 0 then x else x + (toInteger nn) * pow10 !! p) subList

-- call to find', take d (digit) and find all the integers specified by []
find :: Int -> [Integer]
find d = find'' d 0 (0, 0, 0, 10, -1)


showtime :: Integer -> String
showtime t = showFFloat (Just 3) (fromIntegral t / (10^12)) " seconds"

main :: IO ()
main = do
	t <- getCPUTime
	answer <- return $ sum $ map (sum . find) [1..9]
	putStrLn $ show answer
	t' <- getCPUTime
	putStrLn $ "Time: " ++ showtime(t' - t)