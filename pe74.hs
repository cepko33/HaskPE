import UsefulSnippets

fact i = product[1..i]

digFac i = sum $ map (fact) $ listNum i 

nonRep :: Integer -> [Integer] -> Integer
nonRep dig terms
	| dig `elem` terms = 0
	| otherwise = 1 + nonRep (digFac dig) (dig:terms) 

main = print $ sum [1| x<-[1..1000000], nonRep x [] == 60]
