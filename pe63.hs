import UsefulSnippets

digiCou :: Int -> Int -> Int
digiCou _ 20 = 0
digiCou base power
	| size == power = 1 + digiCou (base + 1) power
	| size > power = digiCou 1 (power + 1)
	| otherwise = digiCou (base + 1) power
	where 
	num = (base^power)
	size = length $ listNum num
