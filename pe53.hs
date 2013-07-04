rFromN :: Int -> Int -> Int
rFromN r n = 
    let	fact 0 = 1
	fact i = product [1..i]
    in (fact n) `div` ((fact r) * (fact (n-r)))

main = print $ sum [ 1 | n<-[1..100], r<-[1..(n-1)], 1000000 < rFromN r n]

