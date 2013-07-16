import UsefulSnippets

pe38' :: Integer -> Integer -> [Integer] -> [Integer]
pe38' i n acc
	| length acc > 9 = []
	| length acc == 9 = acc
	| otherwise = pe38' i (n+1) (acc ++ (listNum num))
	where 
	num = i*n

pe38 i = pe38' i 1 []

main = print $ maximum [cn | i<-[5..100000], let num = pe38 i, isPandig num, let cn = comb num, cn > 800000000]
