import UsefulSnippets
import Data.List ((\\), nub, foldl', sortBy)
import Data.Ord (comparing)

isPerm :: Integer -> Integer -> Bool
isPerm x1 x2 = (listNum x1) \\ (listNum x2) == []

euler_fi n = let 
   fs = nub $ factorize n 
   pr = n * product [p-1 | p <- fs] 
  in foldl' div pr fs

factorize n | n > 1 = go n primes  where
  go n ds@(d:t)
    | d*d > n    = [n]
    | r == 0     =  d : go q ds
    | otherwise  =      go n t
        where 
          (q,r)  = quotRem n d

primeL = takeWhile (< 10000) primes

pe70 [] _ = []
pe70 (px:pxs) [] = pe70 pxs pxs
pe70 (px:pxs) (py:pys) 
	| x > 10^7 = pe70 pxs pxs
	| isPerm x t = (x, ((fromInteger x)/(fromInteger t))):pe70 (px:pxs) pys
	| otherwise = pe70 (px:pxs) pys
	where 
	x = px*py
	t = fromIntegral $ euler_fi x

main = print $ fst $ head $ sortBy (comparing snd) $ pe70 primeL primeL
