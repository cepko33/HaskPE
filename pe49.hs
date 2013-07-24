import UsefulSnippets
import Data.List

prlist = takeWhile (<= 9999) $ dropWhile (< 1000) primes

ascPerm i = sort $ filter (> 1000) $ filter isPrime $ map comb $ permutations $ listNum i

primeSeq :: Integer -> Integer -> Integer -> Bool
primeSeq a b c = c-b == b-a

isSeq :: [Integer] -> [Integer] -> [Integer] -> [Integer]
isSeq [] _ _ = []
isSeq (x:xs) [] [] = isSeq xs xs xs
isSeq (x:xs) (y:ys) [] = isSeq (x:xs) ys ys
isSeq (x:xs) (y:ys) (z:zs)
	| y == z = isSeq (x:xs) (y:ys) zs
	| x == y = isSeq (x:xs) ys ys
	| primeSeq x y z = x:y:z:[]
	| otherwise = isSeq (x:xs) (y:ys) zs

pe49 [] = []
pe49 (px:pxs) = 
	let lst = ascPerm px
	in [isSeq lst lst lst] ++ pe49 pxs


main = print $ pe49 prlist
