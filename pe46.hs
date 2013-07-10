import UsefulSnippets


goldbach :: Integer -> [Integer] -> Integer
goldbach i plist
	| isPrime i = goldbach (i+2) plist
	| goldbach' i plist = i 
	| otherwise = goldbach (i+2) plist

goldbach' :: Integer -> [Integer] -> Bool
goldbach' i (p:ps)
	| i < p = True
	| isSquare ((i-p) `div` 2) = False
	| otherwise = goldbach' i ps

isSquare :: Integer -> Bool
isSquare i = 
	let sq = sqrt (fromInteger i) 
	in (fromIntegral $ floor sq) == sq

main = print $ goldbach 3 primes

