import Data.List

comb' :: [Int] -> Int
comb' [] = 0
comb' (x:xs) = x + (10 * (comb' xs))

comb :: [Int] -> Int
comb [] = 0
comb list = comb' (reverse list)

listNum :: Int -> [Int]
listNum i
	| i < 10 = [i]
	| otherwise = (listNum (i `div` 10)) ++ [(mod i 10)]

sieveOfE' :: [Int] -> [Int]
sieveOfE' [] = []
sieveOfE' (x:xs) = [x] ++ sieveOfE' [a | a <- xs, (mod a x) /= 0]

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs

pLessMill = takeWhile (< 1000000) primes

pLessThou = takeWhile (< 1000) primes

allRotations :: Int -> [Int]
allRotations i 
	| i >= 0 = allRotations' list (length list)
	| otherwise = [0]
	where list = listNum i

allRotations' :: [Int] -> Int -> [Int]
allRotations' [] i = []
allRotations' (x:xs) 0 = [] 
allRotations' (x:xs) i = [comb(xs ++ [x])] ++ allRotations' (xs++[x]) (i-1)

allIn :: [Int] -> [Int] -> Bool
allIn [] list = True
allIn (x:xs) list
	| elem x list = allIn xs list
	| otherwise = False

filterList :: [Int] -> [Int] -> [Int]
filterList [] list = list
filterList xs list = [a | a <- list, (not (elem a xs))]

circPrimes' :: [Int] -> [Int] -> [Int]
circPrimes' [] retList = nub retList
circPrimes' (x:xs) retList
	| (allIn alro ([x]++xs)) = circPrimes' (filterList alro xs) retList ++ alro 
	| otherwise = circPrimes' xs retList
	where alro = (allRotations x)
--length (circPrimes' (takeWhile (< 1000000) primes) [])
--
