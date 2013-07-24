len :: Integer -> Integer -> Double
len x y = sqrt (fi x^2 + fi y^2); fi = fromIntegral

isRight :: (Integer,Integer) -> (Integer,Integer) -> Bool
isRight (x1,y1) (x2,y2) 
	| x1 + y1 == 0 || x2 + y2 == 0 = False
	| sameAngle x1 y1 x2 y2 = False
	-- | len1 == len2 || len1 == len3 || len2 == len3 = True
	| len1  > len2 && len1 > len3 = isPyth len2 len3 len1
	| len2 > len1 && len2 > len3 = isPyth len1 len3 len2
	| otherwise = isPyth len1 len2 len3
	where len1 = len x1 y1
	      len2 = len x2 y2
	      len3 = len (x1-x2) (y1-y2)


sameAngle :: Integer -> Integer -> Integer -> Integer -> Bool
sameAngle x1 y1 x2 y2 = (fi x1 / fi y1) == (fi x2 / fi y2)       

isPyth :: Double -> Double -> Double -> Bool
isPyth a b c = fi (floor (a^2) + floor (b^2)) == fi (floor $ c^2)
 
rmRevs _ [] = []
rmRevs ((x1, y1),(x2, y2)) (x:xs)
	| x == ((x2, y2), (x1, y1)) = xs
	| otherwise = x:rmRevs ((x1, y1),(x2, y2)) xs

noRevs [] = []
noRevs (x:xs) = x:noRevs (rmRevs x xs)
