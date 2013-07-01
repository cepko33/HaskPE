listDiv :: Int -> [Int]
listDiv 1 = [1]
listDiv i = listDiv_w i (i-1)

listDiv_w :: Int -> Int -> [Int]
listDiv_w _ 1 = [1]
listDiv_w i f 
	| (mod i f) == 0 = listDiv_w i (f-1) ++ [f]
	| otherwise = listDiv_w i (f-1)

abund :: Int -> Char
abund i
	| divSum < i = 'd'
	| divSum > i = 'a'
	| divSum == i = 'p'
	where divSum = (sum (listDiv i))
