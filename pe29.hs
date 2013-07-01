list = [a^b | a<-[2..100], b<-[2..100]]

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs) = 
	if x `elem` xs
		then remDup xs
	else 
		remDup xs ++ [x]

main = length (remDup list)