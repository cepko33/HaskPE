pDrome :: (Ord a) => [a] -> Bool 
pDrome [] = True 
pDrome xs
	| length xs == 1 = True
	| head xs == last xs = pDrome ((tail . init) xs)
	| otherwise = False 


decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a


listNum :: Int -> [Int]
listNum i
        | i < 10 = [i]
        | otherwise = (listNum (i `div` 10)) ++ [(mod i 10)]

pe36 :: Int -> Int
pe36 0 = 0
pe36 i 
	| (pDrome (listNum i)) && (pDrome (decToBin i)) = i + pe36 (i - 1)
	| otherwise = pe36 (i - 1)
