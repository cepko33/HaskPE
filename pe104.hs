import UsefulSnippets
import Data.List
import Data.Bits

takeLast 0 list = []
takeLast _ [] = []
takeLast i list = takeLast (i-1) (init list) ++ [last list]

pe104' i fst snd
	| not (isPandig lastNine) = pe104' (i+1) snd next
	| not (isPandig firstNine) = pe104' (i+1) snd next
	| otherwise = i
	where 
	next = fst+snd
	lastNine = listNum $ next `mod` 10^9
	firstNine = listNum $ getNineFirst next

getNineFirst n = n `div` 10^(((length . show) n) - 9)

main = print $ pe104' 3 1 1
