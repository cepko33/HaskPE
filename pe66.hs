import Data.Ratio

deq x y d = (x^2)-(d*y^2)


cfe' :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
cfe' m d a s 0 = [a]
cfe' m d a s i = 
	let
	mn1 = d*a - m
	dn1 = (s - mn1^2) `div` d
	an1 = floor ((sqrt (fromIntegral s) + (fromIntegral mn1)) / (fromIntegral dn1))
	in a:cfe' mn1 dn1 an1 s (i-1)

cfe s i = cfe' 0 1 (floor $ (sqrt $ fromIntegral s)) s i

findDeq' x y d
	| eq == 1 = x
	| eq > 1 = findDeq' x (y+1) d
	| eq < 1 = findDeq' (x+1) (y-1) d
	where eq = deq x y d

findDeq'' :: Integer -> Integer -> Integer
findDeq'' y d
	| isSquare num = floor $ sqrt $ fromIntegral num
	| otherwise = findDeq'' (y+1) d
	where num = 1 + (d * y^2)

findDeq d = findDeq'' 1 d 

isSquare :: Integer -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x



main = print $ maximum [findDeq d | d<-[1..1000], not $ isSquare d]
