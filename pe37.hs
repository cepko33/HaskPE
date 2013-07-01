import Data.List
comb' :: [Int] -> Int
comb' [] = 0
comb' (x:xs) = x + (10 * (comb' xs))

comb :: [Int] -> Int
comb [] = 0
comb list = comb' (reverse list)

allIn :: [Int] -> [Int] -> Bool
allIn [] list = True
allIn (x:xs) list
	| x == 0 = allIn xs list
	| elem x list = allIn xs list
	| otherwise = False

listNum :: Int -> [Int]
listNum i
        | i < 10 = [i]
        | otherwise = (listNum (i `div` 10)) ++ [(mod i 10)]

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs

pe37 :: [Int] -> [Int] -> [Int]
pe37 [] _ = []
pe37 (x:xs) primes
	| (allIn initnum primes) && (allIn tailnum primes) = [x] ++ pe37 xs primes
	| otherwise = pe37 xs primes
	where num = listNum x
	      initnum = map comb (inits num)
	      tailnum = map comb (tails num) 
