import UsefulSnippets

pe206 :: Integer -> Integer
pe206 i
	|i2 < 10^18 = pe206 i2
	|(ln !! 2 == 2) = i2
	|otherwise =  pe206 i+10000
	where 
	i2 = i^2
	ln = listNum i2
