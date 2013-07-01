sectionSum n = n^2 + (n^2 - (n - 1)) + (n^2 - (2*n - 2)) + (n^2 - (3*n - 3))

spiralSum n = if n == 1 then 1
	else if even n then spiralSum (n - 1)
	else sectionSum n + spiralSum (n - 2)

main = putStrLn (show (spiralSum 1001))