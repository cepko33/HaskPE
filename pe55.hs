import UsefulSnippets

intrev i = comb $ reverse $ listNum i

lychrelTest :: Integer -> Integer -> Integer
lychrelTest 50 _ = 0
lychrelTest 0 num = lychrelTest 1 (num + (intrev num))
lychrelTest i num
	| pDrome $ listNum num = num
	| otherwise = lychrelTest (i+1) (num + (intrev num))

main = print $ sum [ 1 | x<-[1..9999], let y = lychrelTest 0 x, y == 0 ]
