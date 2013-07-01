maybeCancel :: (Int, Int) -> (Maybe (Int, Int))
maybeCancel (x,y) 
	| x == y = Nothing
	| (mod x 10) == (mod y 10) = Just (x `div` 10, y `div` 10)
	| (mod x 10) == (y `div` 10) = Just (x `div` 10, (mod y 10))
	| (x `div` 10) == (mod y 10) = Just ((mod x 10), y `div` 10)
	| (x `div` 10) == (y `div` 10) = Just ((mod x 10), (mod y 10))
	| otherwise = Nothing

validCancel :: (Maybe (Int, Int)) -> (Int, Int) -> Bool
validCancel Nothing (_,_) = False
validCancel (Just (x,y)) (z,a) = (fromIntegral x)/(fromIntegral y) == (fromIntegral z)/(fromIntegral a)

--[(x,y) | y <- [11..99], x <- [11..y], (mod x 10) > 0, (mod y 10) > 0, validCancel (maybeCancel (x,y)) (x,y)]

--[(16,64),(26,65),(19,95),(49,98)]

--387296
----------
--38729600
