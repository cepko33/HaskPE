import UsefulSnippets
import Data.List (nub)
consSq x y = sum $ map (^2) [x..y]

pe125 :: Integer -> Integer -> Integer -> [Integer]
pe125 x y lim
	| x > (round $ sqrt $ fromIntegral lim) = []
	| num > lim = pe125 (x+1) (x+2) lim
	| pDrome $ listNum num = num:pe125 x (y+1) lim
	| otherwise = pe125 x (y+1) lim
	where num = consSq x y

main = print $ sum $ nub $ pe125 0 2 (10^8)
