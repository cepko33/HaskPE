import Prelude hiding (take,drop)
import Data.Text as T
import Data.List as L
splitFile :: String -> IO [Text]
splitFile fileName = do
	file <- readFile fileName
	return (splitOn (pack "\n") (pack file))
	
emptyTx = pack ""

textToNums :: Text -> [Int]
textToNums txt 
	| txt == emptyTx = []
	| otherwise = [read (T.unpack (T.take 2 txt)) :: Int] ++ (textToNums $ T.drop 3 txt)

t2n :: [Text] -> [[Int]]
t2n [] = []
t2n (x:xs) = (textToNums x):t2n xs

get4Tri :: [[Int]] -> Int -> Int-> [[Int]]
get4Tri _ _ 5 = []
get4Tri (x:xs) i row = ((L.take row . L.drop (i-1)) x) : get4Tri xs i (row+1)

lorr :: [[Int]] -> Int -> Char
lorr (x:xs) i
	| a>b = 'l'
	| otherwise = 'r'	
	where
	a = pathIndex xs i
	b = pathIndex xs (i+1)

pathIndex :: [[Int]] -> Int -> [Int]
pathIndex (x:[]) i = [(x !! i)]
pathIndex (x:xy:xs) i
	| (xy !! i) > (xy !! (i+1)) = [(x !! i)] ++ pathIndex (xy:xs) i
	| otherwise = [(x !! i)] ++ pathIndex (xy:xs) (i+1)
	
highestPath :: [[Int]] -> Int -> Int
highestPath [[]] _ = 0
highestPath (x:[]) i = x !! i
highestPath (x:xs) i
	| lr == 'l' = (x !! i) + highestPath xs i
	| lr == 'r' = (x !! i) + highestPath xs (i+1)
	where lr = lorr xs i

main = do
	file <- splitFile "triangle.txt"
	let triangle = t2n file
	print $ highestPath triangle 0 
