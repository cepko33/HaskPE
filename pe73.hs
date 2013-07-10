import Data.Ratio
import UsefulSnippets

pe73 :: Int -> Int -> Int -> Int
pe73 ret _ 12001 = ret
pe73 ret num den
	| comp <= (1%3) = pe73 ret (num + 1) den
	| comp >= (1%2) = pe73 ret ((den + 1) `div` 3) (den + 1)
	| otherwise = pe73 (ret+1) (num + 1) den 
	where 
	comp = num % den


pe73' :: Integer -> Integer -> Integer
pe73' _ 401 = 0
pe73' num den
	| isPrime den = (den `div` 2) - ((den `div` 3)) + pe73' ((den+1) `div` 3) (den + 1) 
	| otherwise = pe73' 1 (den + 1)
--	| den `mod` num == 0 = pe73' (num+1) den
--	| comp <= (1%3) = pe73' (num+1) den
--	| comp >= (1%2) = pe73' ((den+1) `div` 3) (den+1)
--	| otherwise = 1 + pe73' (num+1) den
	where
	mod2 = den `mod` 2 == 0
	comp = num % den
