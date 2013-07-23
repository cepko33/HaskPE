import UsefulSnippets

pe72 1 = 0
pe72 i = euler_fi i + pe72 (i-1)

pe72' 1 = 2
pe72' n = ((n^2 + 3*n) `div` 2) - (foldl (\acc x -> acc + pe72' (n `div` x)) 0 [2..n]) 

main = print $ pe72 1000000
