import UsefulSnippets
import Data.List ((\\))

pe62' :: Integer -> Integer -> Integer -> Integer
pe62' base _ 4 = base^3
pe62' base check count 
	| length lnc3 > length lnb3 = pe62' (base+1) (base+2) 0
	| perm = pe62' base (check+1) (count+1)
	| otherwise = pe62' base (check+1) count 
	where 
	perm = lnb3 \\ lnc3 == []
	b3 = base^3
	c3 = check^3
	lnb3 = listNum b3
	lnc3 = listNum c3

main = print $ pe62' 1000 1001 0
