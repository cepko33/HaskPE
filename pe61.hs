import UsefulSnippets
import Data.List
oct n = n*(3*n-2)

octList = map (nub . (filter (> 999)) . (map comb) . permutations . listNum)$ takeWhile (<= 9999) $ dropWhile (< 1000) $ [oct n | n<-[1..]]

isHept n lim
	| hep > lim = False
	| hep == lim = True
	| otherwise = isHept (n+1) lim
	where hep = n*(5*n-3) `div` 2

isHex n lim
	| hex > lim = False
	| hex == lim = True
	| otherwise = isHex (n+1) lim
	where hex = n*(2*n-1)

isPen n lim
	| pen > lim = False
	| pen == lim = True
	| otherwise = isPen (n+1) lim
	where pen = n*(3*n-1) `div` 2

isSqu n lim
	| squ > lim = False
	| squ == lim = True
	| otherwise = isSqu (n+1) lim
	where squ = n^2

isTri n lim
	| tri > lim = False
	| tri == lim = True
	| otherwise = isTri (n+1) lim
	where tri = n*(n+1) `div` 2


cycFig :: Integer -> [Integer] -> [Integer]
cycFig n endl
	| isHept 1 n = delete 7 endl
	| isHex 1 n = delete 6 endl
	| isPen 1 n = delete 5 endl
	| isSqu 1 n = delete 4 endl
	| isTri 1 n = delete 3 endl
	| otherwise = endl

pe61' :: [Integer] -> [Integer] -> [Integer] -> [Integer]
pe61' [] figs _ = figs
pe61' (x:xs) figs ret
	| list == [] = x:ret
	| (length figs) > (length list) = pe61' xs list (x:ret)
	| otherwise = pe61' xs figs ret
	where list = cycFig x figs

pe61 :: [[Integer]] -> [[Integer]]
pe61 [] = []
pe61 (x:xs) = (pe61' x [3..7] []): pe61 xs 
	-- | ret == [] = pe61 xs
	-- | otherwise = ret
	-- where ret = pe61' x [3..7] []
