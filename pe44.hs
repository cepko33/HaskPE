pent :: [Int]
pent = [(x*(3*x-1)) `div` 2 | x<-[1..]]  

main = putStrLn $ show $ minimum [x-y | let p = (takeWhile (< 10000000) pent), x<-p, y<-(takeWhile (< x) pent), elem (x+y) p, elem (x-y) p]

