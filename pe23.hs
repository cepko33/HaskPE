import Data.List
listDiv :: Int -> [Int]
listDiv 1 = [1]
listDiv i = listDiv_w i (i-1)

listDiv_w :: Int -> Int -> [Int]
listDiv_w _ 1 = [1]
listDiv_w i f 
	| (mod i f) == 0 = listDiv_w i (f-1) ++ [f]
	| otherwise = listDiv_w i (f-1)

abund :: Int -> Char
abund i
	| divSum < i = 'd'
	| divSum > i = 'a'
	| divSum == i = 'p'
	where divSum = sum $ divisors i 

divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

isMultiple :: Int -> Int -> Int -> Bool
isMultiple i m lim
	| r > lim = False
	| r == lim = True
	| otherwise = isMultiple i (m+1) lim
	where r = i*m

multipleInArr :: Int -> [Int] -> Bool
multipleInArr _ [] = False
multipleInArr i (x:xs)
	| isMultiple x 1 i  = True 
	| otherwise = multipleInArr i xs

allMultiples :: Int -> Int -> Int -> [Int]
allMultiples base mult lim
	| duh <= lim = [duh] ++ allMultiples base (mult + 1) lim
	| otherwise = []
	where duh = (base*mult)

genAb :: Int -> Int -> [Int] -> [Int]
genAb i lim ret
	| i > lim = ret
	| i `elem` ret = genAb (i+1) lim ret 
--	| multipleInArr i bases = genAb (i+1) lim bases ret
	| abund i == 'a' = genAb (i+1) lim (ret ++ (allMultiples i 1 lim))
	| otherwise = genAb (i+1) lim ret

genAb' :: Int -> [Int]
genAb' lim = nub $ sort $ genAb 12 lim []
