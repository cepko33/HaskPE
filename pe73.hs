import Data.Ratio
import UsefulSnippets

farey' a b c d n
	| c > n = []
	| otherwise = (c % d):farey' c d (k*c - a) (k*d - b) n
	where k = (n + b) `div` d

farey n = farey' 0 1 1 n n

main = print $ length $ takeWhile (< (1 % 2)) $ dropWhile (<= (1 % 3)) $ farey 12000
