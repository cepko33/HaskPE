rght :: Int -> [(Int, Int, Int)]
rght lim = [(x,y,z) | z <- [1..(lim-2)], y <- [1..z], x <- [1..y], x+y+z == lim, x^2 + y^2 == z^2]


primTrip :: Int -> [(Int, Int, Int)]
primTrip lim = [(a*k,b*k,c*k) | k <- [1..lim], m <- [1..lim], n <- [1..m], let a = m^2 - n^2, let b = 2*m*n, let c = m^2 + n^2, (gcd m n) == 1, (n+m) `mod` 2 == 1, a*k+b*k+c*k <= 1000]

underThou = primTrip 150

tallyPerim :: Int -> [(Int,Int,Int)] -> Int
tallyPerim _ [] = 0 
tallyPerim i ((a,b,c):xs)
	| a+b+c == i = 1 + tallyPerim i xs
	| otherwise = tallyPerim i xs

--[(x,y) | x <- [1..1000], let y = tallyPerim x underThou, y > 5]
--
