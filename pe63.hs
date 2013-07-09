import UsefulSnippets

digiCou :: Integer -> Integer -> Integer
digiCou _ 0 = 0
digiCou base power
	| size == power = 1 + digiCou (base + 1) power
	| size > power = digiCou 1 (power - 1)
	| otherwise = digiCou (base + 1) power
	where 
	num = (base^power)
	size = toInteger $ length $ listNum num

main = print $ digiCou 1 21
