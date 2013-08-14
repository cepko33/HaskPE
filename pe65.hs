
import UsefulSnippets
import Data.Ratio

eNum' n lim prev ret
	| n > lim = ret
	| n `mod` 3 == 0 = eNum' (n+1) lim ret (ret * 2 * (n `div` 3) + prev)
	| otherwise = eNum' (n+1) lim ret (ret + prev)

eNum n = eNum' 2 n 1 2 

main = print $ sum $ listNum $ eNum 100
