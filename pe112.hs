import UsefulSnippets

bounce' :: [Integer] -> Char -> Char
bounce' (i:[]) c = c
bounce' (i1:i2:ix) c  
	| c == 'i' && i1 > i2 = 'b'
	| c == 'd' && i1 < i2 = 'b'
	| c == ' ' && i1 < i2 = bounce' (i2:ix) 'i'
	| c == ' ' && i1 > i2 = bounce' (i2:ix) 'd'
	| otherwise = bounce' (i2:ix) c	

bounce i = 'b' == bounce' (listNum i) ' '

pe112 :: Integer -> Integer -> Integer
pe112 i cou 
	| ratio (fromIntegral i) (fromIntegral cou) == 0.99 = i
	| bounce i = pe112 (i+1) (cou+1)
	| otherwise = pe112 (i+1) cou
	where 
	ratio _ 0 = 0.0
	ratio a b = (b/a)


main = print $ pe112 1 1
