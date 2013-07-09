import UsefulSnippets
squDig i = sum $ map (^2) $ listNum i

sdc :: Integer -> [Integer] -> ([Integer], Bool)
sdc i arr  
	| i `elem` arr = ([],True)
	| i == 1 = ([], False)
	| i == 89 = ([], True)
	| snd $ cds = (i:(fst cds), snd cds)
	| otherwise = ([], False)
	where cds = sdc (squDig i) arr

sdcToLim :: Integer -> Integer -> [Integer] -> [Integer]
sdcToLim i lim dict
	| (sum $ listNum i) == 1 = sdcToLim (i+1) lim dict
	| i > lim = []
	| snd $ cds = i:sdcToLim (i+1) lim (dict++(fst cds))
	| otherwise = sdcToLim (i+1) lim dict
	where cds = sdc (squDig i) dict

main = print $ length $ sdcToLim 1 10000000 []
