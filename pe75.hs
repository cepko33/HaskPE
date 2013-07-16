import Data.Text as T
data Point = Point Float Float deriving (Show)
data Triangle = Triangle Point Point Point deriving (Show)

px :: Point -> Float
px (Point x _) = x

py :: Point -> Float
py (Point _ y) = y

sign :: Point -> Point -> Point -> Float
sign p1 p2 p3 = ((px p1 - px p3) * (py p2 - py p3)) - ((px p2 - px p3) * (py p1 - py p3))

inTriangle' :: Point -> Point -> Point -> Point -> Bool
inTriangle' point t1 t2 t3 =
	let
	b1 = sign point t1 t2 < 0.0
	b2 = sign point t2 t3 < 0.0
	b3 = sign point t3 t1 < 0.0
	in
	(b1 == b2) && (b2 == b3)

inTriangle :: Point -> Triangle -> Bool
inTriangle point (Triangle p1 p2 p3) = inTriangle' point p1 p2 p3

textToTri :: [Text] -> [Triangle]
textToTri [] = []
textToTri (x:xs) = 
	let
	coords = Prelude.map T.unpack (splitOn (pack ",") x)
	conv t = read (coords !! t) :: Float
	p1x = conv 0
	p1y = conv 1
	p2x = conv 2
	p2y = conv 3
	p3x = conv 4
	p3y = conv 5
	in
	(Triangle (Point p1x p1y) (Point p2x p2y) (Point p3x p3y)):textToTri xs

splitFile :: String -> IO [Text]
splitFile fileName = do
        file <- readFile fileName
        return (splitOn (pack "\n") (pack file))

main = do
	file <- splitFile "triangles.txt"
	print $ Prelude.foldl (\acc tri -> if inTriangle (Point 0.0 0.0) tri then (acc + 1) else acc) 0 $ textToTri (Prelude.init file)
