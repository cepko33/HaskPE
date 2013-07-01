import Data.List
genDiag :: Int -> [Int]
genDiag 1 = [1]
genDiag i =
	let 
	n = ((2*i)-1)^2
	dif = (2*(i-1))
	in [n-3*dif,n-2*dif,n-dif,n]

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs

pe58 :: Int -> [Int] -> Int
pe58 1 _ = pe58 2 [1] 
pe58 i justPrimes
	| ((fromIntegral (length newJup)) / (fromIntegral (4*(i-1) + 1))) < 0.15= (2*i-1)
	| otherwise = pe58 (i+1) newJup
	where 
	newBit = genDiag i
	newJup = justPrimes ++ filter (`elem` (takeWhile (< last newBit) primes)) newBit

