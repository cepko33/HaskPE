module UsefulSnippets where
import Data.List

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

isPandig :: [Integer] -> Bool
isPandig [] = False
isPandig tot = [1..9] \\ tot == []

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

euler_fi n = let
   fs = nub $ factorize n
   pr = n * product [p-1 | p <- fs]
  in foldl' div pr fs

factorize n | n > 1 = go n primes  where
  go n ds@(d:t)
    | d*d > n    = [n]
    | r == 0     =  d : go q ds
    | otherwise  =      go n t
        where
          (q,r)  = quotRem n d

sumDiv' [] nums divs = nums `div` divs
sumDiv' (fx:fxs) nums divs = 
	let 
	l = length fx
	n = head fx
	in
	sumDiv' fxs (nums * ((n^(l+1))-1)) (divs * (n - 1))	

sumDiv 1 = 1
sumDiv n = (sumDiv' (group $ factorize n) 1 1) - n
