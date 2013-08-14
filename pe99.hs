import Data.Text as T hiding (head, last, map)
import Data.Text.Read as R

exp' :: Integer -> Integer -> Double
exp' x y = (fromIntegral y) * (log $ fromIntegral x)

splitFile :: String -> IO [Text]
splitFile fileName = do
        file <- readFile fileName
        return (splitOn (pack "\n") (T.filter (/='"') (pack file)))

readInt = read :: String -> Integer

baseExp :: Text -> (Integer, Integer)
baseExp pair = 
	let num = map unpack $ splitOn (pack ",") pair
	in (readInt $ head num, readInt $ last num)

pe99 :: [(Integer,Integer)] -> Integer -> Integer -> Double -> Integer
pe99 [] _ n _ = n
pe99 ((xx,xy):xs) i n reg
	| test > reg = pe99 xs (i+1) i test
	| otherwise = pe99 xs (i+1) n reg 
	where test = exp' (fromIntegral xx) xy

main = do
	file <- splitFile "base_exp.txt"
	print $ pe99 (map baseExp file) 1 0 0
