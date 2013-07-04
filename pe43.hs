import Data.List
import UsefulSnippets

numbers = permutations [0..9]

divisible a b = a `mod` b == 0

sublist :: Int -> Int -> [Int] -> [Int]
sublist _ _ [] = []
sublist start end arr = take (end-start+1) $ drop start arr

valid43 :: [Int] -> Bool
valid43 [] = False
valid43 numArr = 
	let 
	sub n = comb $ sublist n (n+2) numArr
	divi i j = divisible (sub (i-1)) j
	in
	(divi 2 2 && divi 3 3 && divi 4 5 && divi 5 7 && divi 6 11 && divi 7 13 && divi 8 17)	 

main = print $ sum [comb x | x<- numbers, valid43 x]
