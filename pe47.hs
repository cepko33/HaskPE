import Data.List
primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs

primeL = takeWhile (< 1000) primes

primeFactor :: Int -> [Int] -> [Int]
primeFactor 1 _ = []
primeFactor i [] = [i]
primeFactor i (x:xs)
	| 0 == i `mod` x = [x] ++ primeFactor (i `div` x) (x:xs)
	| otherwise = primeFactor i xs

countFour :: Int -> [Int]
countFour 0 = []
countFour i
	| check i && check (i+1) && check (i+2) && check (i+3) = [i..(i+3)]
	| otherwise = countFour (i+1)
	where check i = length (nub (primeFactor i primeL)) == 4

main = putStrLn $ show $ countFour 100000
