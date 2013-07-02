import Data.Char
import Data.Text hiding (length, all, head, take, tail)
import Data.Bits

splitFile :: String -> IO [Text]
splitFile fileName = do
        file <- readFile fileName
        return (splitOn (pack ",") (Data.Text.filter (/='"') (pack file)))

textToInt :: [Text] -> [Int]
textToInt [] = []
textToInt (x:xs) = [read (unpack x) :: Int] ++ textToInt xs

intPrint :: [Int] -> [Char] 
intPrint [] = []
intPrint (x:xs) = [chr x] ++ intPrint xs

file = splitFile "cipher1.txt"

isAlpha' x = isSpace x || isAlphaNum x

contains :: [Char] -> [Char] -> Bool
contains contStr exStr
	| len > (length contStr) = False
	| exStr == take len contStr = True
	| otherwise = contains (tail contStr) exStr
	where len = length exStr 

xorInt :: [Int] -> Int -> Int -> Int -> [Int]
xorInt [] _ _ _ = []
xorInt arr a b c 
	| length arr  == 2 = (xor (arr !! 0) a):(xor (arr !! 1) b):[]
	| length arr  == 1 = (xor (arr !! 0) a):[]
	| otherwise = (xor (arr !! 0) a):(xor (arr !! 1) b):(xor (arr !! 2) c):xorInt (Prelude.drop 3 arr) a b c 

intToXorString :: [Text] -> Int -> Int -> Int -> [Char]
intToXorString [] _ _ _ = []
intToXorString arr a b c = intPrint (xorInt (textToInt arr) a b c)

main = do
	fileout <- file
	let text = (head [string | x<-[97..122], y<-[97..122], z<-[97..122], let string = intToXorString fileout x y z, contains string "of", contains string "the", contains string "is", contains string "that"])
	print $ sum[ord(x) | x<-text]



--print [a : b : c : d : e : f : [] | x<-[97..122], y<-[97..122], z<-[97..122], let a = chr (xor (ints !! 0) x), let b = chr (xor (ints !! 1) y), let c = chr (xor (ints !! 2) z), let d = chr (xor (ints !! 3) x), let e = chr (xor (ints !! 5) y), let f = chr (xor (ints !! 6) z), isAlpha' a, isAlpha' b, isAlpha' c, isAlpha' d, isAlpha' e, isAlpha' f] 
