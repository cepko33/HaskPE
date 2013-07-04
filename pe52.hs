import UsefulSnippets
import Data.List

pe52 :: Int -> Bool
pe52 n 
	| mult 2 = False
	| mult 3 = False
	| mult 4 = False
	| mult 5 = False
	| mult 6 = False
	| otherwise = True
	where mult i = (listNum n) \\ (listNum (n*i)) /= []

main = print $ head $ take 1 [x | x<-[1..], pe52 x]
