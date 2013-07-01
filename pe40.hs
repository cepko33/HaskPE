import Data.Char
generateHugeString' :: Int -> Int -> [Char]
generateHugeString' x y
	| x == y = show x
	| otherwise = show x ++ generateHugeString' (x+1) y

generateHugeString :: Int -> [Char]
generateHugeString x = generateHugeString' 0 x

ls = generateHugeString 200000

sumItems :: [Int] -> [Char] -> Int
sumItems [] _ = 1
sumItems (x:xs) string = digitToInt (string !! x) * sumItems xs string
