import UsefulSnippets
import Data.Ratio

sqrt2' :: Integer -> Integer -> Integer -> (Integer, Integer)
sqrt2' 0 n d = (n,d)
sqrt2' i n d = sqrt2' (i-1) (n + 2*d) (n + d)

sqrt2 n = sqrt2' n 3 2

pe57 (a,b) = (length $ listNum a) > (length $ listNum b)

main = print $ sum [ 1 | x<-[1..1000], pe57 $ sqrt2 x]
