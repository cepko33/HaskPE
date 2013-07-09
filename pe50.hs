import UsefulSnippets
import Data.Ord
import Data.List
plist = takeWhile (<= 10000) primes

thd :: (a,b,c) -> c
thd (_,_,c) = c
fstt :: (a,b,c) -> a
fstt (a,_,_) = a
sndd :: (a,b,c) -> b
sndd (_,b,_) = b

isPrime' :: Integer -> [Integer] -> Bool
isPrime' n (x:xs) 
	| n < x  = False
	| n == x = True
	| otherwise = isPrime' n xs

isPrime n = isPrime' n primes

primeSum' :: Integer -> Integer -> [Integer] -> [(Integer, Integer, Integer)]
primeSum' _ _ [] = []
primeSum' sum i (x:xs)
	| i < 500 = primeSum' (sum + x) (i+1) xs
	| sum + x > 1000000 = []
	| isPrime $ sum + x= [(i+1, sum+x, x)] ++ primeSum' (sum + x) (i+1) xs
	| otherwise = primeSum' (sum + x) (i+1) xs 

primeSum :: [Integer] -> [(Integer, Integer, Integer)]
primeSum [] = []
primeSum primes = primeSum' 0 0 primes ++ primeSum (tail primes)

main = print $ sndd $ last $ sortBy (comparing fstt) $ primeSum plist
