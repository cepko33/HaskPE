import UsefulSnippets
import Data.List (all)
revert n = comb' $ listNum n

allOdd n = all odd $ listNum n
check n = ((odd n) && (even $ head $ listNum n)) || 
	  ((even n) && (odd $ head $ listNum n))


pe145' n lim
	| n > lim = []
	| (not . check) n = pe145' (n+1) lim
	| n `mod` 10 == 0 = pe145' (n+1) lim
	| allOdd (n + revert n) = n:pe145' (n+1) lim
	| otherwise = pe145' (n+1) lim

main = print [ n | n<-[1..(10^6)], n `mod` 10 > 0, check n, allOdd (n + revert n)]
