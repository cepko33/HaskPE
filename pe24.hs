import Data.List
fac :: Int -> Int
fac x = product[1..x] 

permutationX :: Int -> [Int] -> [Int]
permutationX x nums = permutationX' (x - 1) nums

permutationX' :: Int -> [Int] -> [Int]
permutationX' 0 nums = nums 
permutationX' x nums 
	| length nums == 1 = nums
	|otherwise = [num] ++ (permutationX' (x-rem) (delete num nums))
	where index = permIndex x nums
	      rem = numRem x nums
	      num = nums !! index 

permIndex :: Int -> [Int] -> Int
permIndex x nums = x `div` (fac ((length nums) -1))

numRem :: Int -> [Int] -> Int
numRem x nums = (permIndex x nums) * (fac ((length nums) -1))


main = putStrLn (show $ permutationX 1000000 [0..9])
