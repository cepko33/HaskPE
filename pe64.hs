cfe' :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
cfe' m d a s 0 = [a]
cfe' m d a s i = 
        let 
        mn1 = d*a - m
        dn1 = (s - mn1^2) `div` d
        an1 = floor ((sqrt (fromIntegral s) + (fromIntegral mn1)) / (fromIntegral dn1))
        in a:cfe' mn1 dn1 an1 s (i-1)

cfe s i = cfe' 0 1 (floor $ (sqrt $ fromIntegral s)) s i
