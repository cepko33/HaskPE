deq x y d = (x^2)-(d*y^2)


findDeq' x y d
	| eq == 1 = x
	| eq > 1 = findDeq' x (y+1) d
	| eq < 1 = findDeq' (x+1) 1 d
	where eq = deq x y d

findDeq d = findDeq' 2 1 d 

isSquare :: Int -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

main = print $ maximum [findDeq d | d<-[1..1000], not $ isSquare d]
