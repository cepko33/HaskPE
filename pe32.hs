import Data.List
ninePan :: Int -> Int -> Bool
ninePan	a b = hasAll (prodArr a b) [1..9] 

hasAll :: [Int] -> [Int] -> Bool
hasAll [] [] = True
hasAll (x:xs) [] = False
hasAll [] (x:xs) = False
hasAll (x:xs) set
	| elem x set = hasAll xs (delete x set)
	| otherwise = False

prodArr :: Int -> Int -> [Int]
prodArr a b = (intArr a) ++ (intArr b) ++ (intArr (a*b)) 

intArr :: Int -> [Int] 
intArr x 
	| x < 10 = [x]
	| otherwise = (intArr (x `div` 10)) ++ [mod x 10] 

--sum (nub[z | y<-[1..2000], x<-[1..y], let z = x*y, ninePan x y])
--
