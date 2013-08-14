import UsefulSnippets

pe123 (x:xs:xs2) i
	| remain > 10^10 = i
	| otherwise = pe123 xs2 (i+2)
	where 
	ps = (x-1)^i + (x+1)^i
	remain = ps `mod` x^2

main = print $ pe123 primes 1
