import IO
import Data.Text
import Data.List

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
listScore_w (x:xs) i = (nameScore (uncons x) i) + (listScore_w xs (i+1)) 

main = do
	splitted <- splitFile "names.txt"
	print (listScore (sort splitted))
