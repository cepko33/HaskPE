import UsefulSnippets

list = takeWhile (<= 9999) $ dropWhile (< 1000) primes

cyc' (x:xs) 1 = [x:xs]
cyc' (x:xs) len = [x:xs] ++ cyc' (xs++[x]) (len - 1)

cyc xs = cyc' xs $ length xs


