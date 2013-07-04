import UsefulSnippets

main = print $ maximum [x | a<-[1..100], b<-[1..a], let x = sum $ listNum (a^b)]


