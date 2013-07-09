import UsefulSnippets
import Data.Ord
import Data.List

plist = takeWhile (< 100000) primes

thd :: (a,b,c) -> c
thd (_,_,c) = c
fstt :: (a,b,c) -> a
fstt (a,_,_) = a
sndd :: (a,b,c) -> b
sndd (_,b,_) = b

consecPrime :: Integer -> Integer -> Integer -> Integer
consecPrime a b n
	| num < 2 = n
	| num `elem` plist = consecPrime a b (n+1)
	| otherwise = n
	where num = n^2 + a*n + b

main = do
	let top = last $ sortBy (comparing thd) [(a,b,c) | a<-[-1000..1000], b<-[-1000..1000],let c = consecPrime a b 0, c > 2]
	print $ (fstt top) * (sndd top)
