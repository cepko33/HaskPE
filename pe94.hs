heron a b c = (/4) $ sqrt ((a^2 + b^2 + c^2)^2 - 2*(a^4 + b^4 + c^4))

isWhole x = ceiling x == floor x

pe94' a b c
	| perim > 1000000000 = []
	| isWhole $ heron a b c = (a,b,c):pe94' (a+1) (b+1) (c+1)
	| otherwise = pe94' (a+1) (b+1) (c+1)
	where perim = floor $ a + b + c

pe94 = (pe94' 2 2 3) ++ (pe94' 2 2 1)
