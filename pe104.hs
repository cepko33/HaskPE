import UsefulSnippets

takeLast 0 list = []
takeLast _ [] = []
takeLast i list = takeLast (i-1) (init list) ++ [last list]

pe104' i fst snd
	| not (isPandig $ take 9 list) = pe104' (i+1) snd next
	| not (isPandig $ takeLast 9 list) = pe104' (i+1) snd next
	| otherwise = i
	where 
	next = fst+snd
	list = listNum next

pe104 = pe104' 2 0 1
