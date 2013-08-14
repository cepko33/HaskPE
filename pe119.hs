import UsefulSnippets
import Data.List (sort)

pe11 n i
	| i > 30 = []
	| hasPower sumNum 1 n = n:pe11 (n+1) (i+1)
	| otherwise = pe11 (n+1) i
	where sumNum = sum $ listNum n

hasPower 1 _ _ = False
hasPower n i lim
	| n^i == lim = True
	| n^i > lim = False
	| otherwise = hasPower n (i+1) lim


pe11' 200 = []
pe11' i = isaPower i 2 ++ pe11' (i+1)

isaPower _ 200 = []
isaPower n i
	| num < 10 = isaPower n (i+1)
	| n == ((sum . listNum) num) = num:isaPower n (i+1)
	| otherwise = isaPower n (i+1)
	where num = n^i

main = print $ (sort $ pe11' 2) !! 29
