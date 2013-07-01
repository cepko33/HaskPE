--Digit Factorials
--

listNum :: Int -> [Int]
listNum i
        | i < 10 = [i]
        | otherwise = (listNum (i `div` 10)) ++ [(mod i 10)]

fact :: Int -> Int
fact 0 = 1
fact i = product[1..i]

digFact' :: [Int] -> Int
digFact' [] = 0
digFact' (x:xs) = (fact x) + (digFact' xs)

digFact :: Int -> Int
digFact 0 = 1
digFact i = digFact' (listNum i)

--[x | x<-[3..100000], (digFact x) == x]
