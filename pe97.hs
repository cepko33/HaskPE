twoMultMod x 0 = x
twoMultMod x 1 = ((2*x) `mod` (10^10))
twoMultMod x 2 = ((3*x) `mod` (10^10))
twoMultMod x cou = twoMultMod ((8*x) `mod` (10^10)) (cou-3)

main = print $ (twoMultMod 28433 7830457) + 1
