import Data.List

hasAll :: [Int] -> [Int] -> Bool
hasAll [] [] = True
hasAll (x:xs) [] = False
hasAll [] (x:xs) = False
hasAll (x:xs) set
	| elem x set = hasAll xs (delete x set)
	| otherwise = False

intArr :: Int -> [Int] 
intArr x 
	| x < 10 = [x]
	| otherwise = (intArr (x `div` 10)) ++ [mod x 10] 

isPan :: Int -> Bool
isPan n = 
	let arr = intArr n
	in hasAll arr [1..(length arr)]

primesToGT m = 2 : sieve [3,5..m]
  where
    sieve (p:xs) 
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p /= 0]

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = all (\x -> n `rem` x /= 0) candidates
  where candidates = takeWhile (\x -> x*x <= n) primes
