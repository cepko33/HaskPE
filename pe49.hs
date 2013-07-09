import UsefulSnippets
import Data.List

prlist = takeWhile (<= 9999) $ dropWhile (< 1000) primes

ascPerm i = sort $ map comb $ permutations $ listNum i

primeSeq :: Integer -> Integer -> Integer -> Bool
primeSeq a b c = (b `elem` prlist) && (c `elem` prlist) && c-b == b-a

seqFromPr :: [Integer] -> Integer -> Integer -> Integer -> Bool
seqFromPr list head ia ib
	| ia > (toInteger $ length list)-1 = False
	| ib > (toInteger $ length list)-1 = seqFromPr list head (ia+1) (ia+2)
	| primeSeq head (ind (fromIntegral ia)) (ind (fromIntegral ib)) = True
	| otherwise = seqFromPr list head ia (ib+1)
	where
	ind i = list !! i
