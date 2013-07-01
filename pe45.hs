tri = [sum[1..x] | x<-[1,3..]]
pen = [(x*(3*x-1)) `div` 2 | x<-[1..]]
hex = [x*(2*x-1) | x<-[1..]]

bound = 1600000000

triL = takeWhile (< bound) tri
penL = takeWhile (< bound) pen
hexL = takeWhile (< bound) hex

main = putStrLn $ show $ [x | x<-hexL, elem x penL]
