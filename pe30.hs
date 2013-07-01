import NumArray

sumFifth :: (Integral a) => a -> a
sumFifth num = sum [x^5 | x <- (numToArray num)] 

--[x | x <- [1..100000], x == sumFifth x]
