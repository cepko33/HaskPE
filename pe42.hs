import Data.Text
import Data.List
import IO
tri = [sum[1..x] | x<-[1..]]
triL = Data.List.takeWhile (< 10000) tri

alphaNum = ['A'..'Z']

alphaIndex :: Maybe Int -> Int
alphaIndex (Just i) = i
alphaIndex Nothing = 0

nameScore :: Maybe (Char,Text) -> Int -> Int
nameScore Nothing _ = 0
nameScore (Just (c,t)) loc = (((alphaIndex(elemIndex c alphaNum)) + 1) * loc) + nameScore (uncons t) loc

splitFile :: String -> IO [Text]
splitFile fileName = do
        file <- readFile fileName
        return (splitOn (pack ",") (Data.Text.filter (/='"') (pack file)))

listScore :: [Text] -> Int
listScore l = listScore_w l 1

listScore_w :: [Text] -> Int -> Int
listScore_w [] _ = 0
listScore_w (x:xs) i
	| elem (nameScore (uncons x) 1) triL = 1 + listScore_w xs 1
	| otherwise = listScore_w xs 1

main = do
	splitted <- splitFile "words.txt"
	print (listScore splitted) 
