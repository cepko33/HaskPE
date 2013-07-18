import UsefulSnippets
import Data.List (nub, group, sort)

primeL = takeWhile (<= 7081) primes

pe87 [] _ _ _ = []
pe87 (l:lx) [] _ lim = pe87 lx primeL primeL lim
pe87 (l:lx) (m:mx) [] lim = pe87 (l:lx) mx primeL lim
pe87 (l:lx) (m:mx) (n:nx) lim
	| num > lim = pe87 (l:lx) mx primeL  lim
	| otherwise = num: pe87 (l:lx) (m:mx) nx lim
	where num = (l^4) + (m^3) + (n^2)

pe87' n = pe87 primeL primeL primeL n

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

main = print $ length $ rmdups $ pe87' 50000000
