import Data.Ord

sumDivs' i 1 = 1 + sumDivs' i 2
sumDivs' i x
	| (fromInteger x) > (sqrt (fromInteger i)) = 0 
	| i `mod` x == 0 = x + (i `div` x) + sumDivs' i (x+1)
	| otherwise = sumDivs' i (x+1) 

sumDivs i = sumDivs' i 1

pe95' i [] = pe95' (sumDivs i) [i]
pe95' i ret
	| i == 1 = []
	| i == head ret = ret 
	| i `elem` ret = []
	| otherwise = pe95' (sumDivs i) (ret ++[i])
pe95 i
	| i > 100000 = []
	| list == [] = pe95 (i+1) 
	| otherwise = (i,length list, minimum list):pe95 (i+1)
	where 
	divs = fromIntegral $ sumDivs i
	list = pe95' i []	

