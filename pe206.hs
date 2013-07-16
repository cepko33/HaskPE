import UsefulSnippets

pe206 :: Integer -> Integer
pe206 i
	|(chi 1) && (chi 2) && (chi 3) && (chi 4) && (chi 5) && (chi 6) = i2 
	|otherwise =  pe206 i+1
	where 
	i2 = i^2
	ln = listNum i2
	chi :: Integer -> Bool
	chi n = (ln !! (2*((fromIntegral n)-1)) == n)
