import UsefulSnippets
import Data.Ratio
import Data.List
genDiag :: Integer -> [Integer]
genDiag 1 = [1]
genDiag i =
	let 
	n = ((2*i)-1)^2
	dif = (2*(i-1))
	in [n-3*dif,n-2*dif,n-dif]


pe58 :: Integer -> Integer -> (Integer,Double)
pe58 1 _ = pe58 2 0 
pe58 i primes
	| ratio < 0.10= (i,ratio)
	| otherwise = pe58 (i+1) newJup 
	where 
	newBit = genDiag i
	newJup = primes +  (fromIntegral $ length $ filter stolenIsPrime newBit)
	--newJup = primes + (fromIntegral $ length $ filter isPrime newBit)
	ratio = (fromIntegral newJup) / (fromIntegral (4*(i-1) + 1)) 

stolenIsPrime n
	| n <= 1 = False
	| n == 2 = True
	| n `mod` 2 == 0 = False
	| n < 9 = True
	| n `mod` 3 == 0 = False
	| otherwise = coun n 5

coun n i
	| i^2 > n = True
	| n `mod` i == 0 = False
	| n `mod` (i + 2) == 0 = False
	| otherwise = coun n (i+6)

main = print $ (2 * (fst (pe58 1 0)) - 1)
