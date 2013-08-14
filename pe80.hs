import Data.Number.CReal
import Data.Char

isSquare :: Integer -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

getSqrt n = take 101 $ showCReal 101 (sqrt n) 

addString :: [Char] -> Int
addString [] = 0
addString ('.':xs) = addString xs
addString (x:xs) = (digitToInt x) + addString xs

main = print $ sum [addString $ getSqrt (fromIntegral n) | n <- [1..100], (not . isSquare) n]


