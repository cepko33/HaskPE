import UsefulSnippets
import Data.List

abundance = [x | x<-[12..28122], sumDiv x > x]

isAbundSum i (x:xs)
	| i < x = True 
	| (i-x) `elem` abundance = False
	| otherwise = isAbundSum i xs

pe23' _ [] _ = True
pe23' i (x:xs) ablist
	| i-x `elem` ablist = False
	| otherwise = pe23' i xs ablist

pe23 i lim ret
	| i > lim = []
	| abund && pe23' i ret ret = i: pe23 (i+1) lim (ret++[i])
	| pe23' i ret ret = i: pe23 (i+1) lim ret
	| abund = pe23 (i+1) lim (ret++[i])
	| otherwise = pe23 (i+1) lim ret
	where abund = sumDiv i > i

main = sum $ pe23 1 20162 []
