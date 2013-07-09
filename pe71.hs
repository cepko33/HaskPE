gcm :: Int -> Int -> Int
gcm a b
	| a `mod` b == 0 = b
	| otherwise = gcm (a `div` b) (a `mod` b)
