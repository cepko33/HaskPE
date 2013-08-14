import Data.Ord
import UsefulSnippets

sumDivs' i 1 = 1 + sumDivs' i 2
sumDivs' i x
	| (fromInteger x) > (sqrt (fromInteger i)) = 0 
	| i `mod` x == 0 = x + (i `div` x) + sumDivs' i (x+1)
	| otherwise = sumDivs' i (x+1) 

sumDivs i = sumDivs' i 1

pe95' i [] = pe95' (sumDiv i) [i]
pe95' i ret
	| i > 1000000 = []
	| i == 1 = []
	| i == head ret = ret 
	| i `elem` ret = []
	| otherwise = pe95' (sumDiv i) (ret ++[i])
pe95 i
	| i > 1000000 = []
	| list == [] = pe95 (i+1) 
	| otherwise = (i,length list, minimum list):pe95 (i+1)
	where 
	divs = fromIntegral $ sumDiv i
	list = pe95' i []	

