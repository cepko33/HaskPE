module UsefulSnippets where

listNum :: Integer -> [Integer]
listNum i
        | i < 10 = [i]
        | otherwise = (listNum (i `div` 10)) ++ [(mod i 10)]

primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs

isPrime' :: Integer -> [Integer] -> Bool
isPrime' n (x:xs) 
        | n < x  = False
        | n == x = True
        | otherwise = isPrime' n xs
        
isPrime n = isPrime' n primes 

pDrome :: (Ord a) => [a] -> Bool 
pDrome [] = True 
pDrome xs
	| length xs == 1 = True
	| head xs == last xs = pDrome ((tail . init) xs)
	| otherwise = False 

comb' [] = 0
comb' (x:xs) = x + (10 * (comb' xs))

comb :: [Integer] -> Integer
comb [] = 0
comb list = comb' (reverse list)

isPandig :: [Int] -> Bool
isPandig [] = False
isPandig tot = sum tot == sum [1..9]
