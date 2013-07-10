import Data.Ratio
import Data.List

pe71 :: Ratio Int -> Int -> Int -> Ratio Int
pe71 ret _ 1000001 = ret
pe71 ret num den
	| comp >= (3%7) = pe71 ret 1 (den + 1)
	| comp <= ret = pe71 ret (num + 1) den
	| otherwise = pe71 comp (num + 1) den
	where comp = num % den

main = print $ pe71 (0%1) 0 999995
