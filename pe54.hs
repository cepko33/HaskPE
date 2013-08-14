import Data.List (sortBy, all, groupBy)
import Data.Ord (comparing)

-- a Card is the Size then the Suit, e.g. (Card '5' 's')
data Card = Card Int Char deriving (Show)

size (Card csize _) = csize
suit (Card _ csuit) = csuit

handVal = ["high", "onePair", "twoPair", "toak", "straight", "flush", "fullHouse", "foak", "straightFlush", "RoyalFlush"]

exHand = [(Card 9 'H'), (Card 5 'S'), (Card 10 'D'), (Card 2 'C'), (Card 2 'H')]

sortHand hand = sortBy (comparing size) hand

consec hand start = foldl (\acc x -> acc ++ [size x]) [] hand == [start..(start+4)]

sameSizeCards cards = all (\x -> (size x) == (size $ head cards)) cards

sameSuit hand = all (\x -> suit x == (suit $ head hand)) hand

--------------------------------------------------

royalFlush hand = sameSuit hand && consec hand 10

straightFlush hand = sameSuit hand && consec hand (size $ head hand)

foak hand = (sameSizeCards $ take 4 hand) || (sameSizeCards $ drop 1 hand)

fullHouse hand = ((sameSizeCards $ take 3 hand) && (sameSizeCards $ drop 2 hand)) ||
		 ((sameSizeCards $ take 2 hand) && (sameSizeCards $ drop 3 hand))

flush hand = sameSuit hand

straight hand = consec hand (size $ head hand)

toak hand = (sameSizeCards $ take 3 hand) || 
	    (sameSizeCards $ take 3 $ drop 1 hand) ||
	    (sameSizeCards $ drop 2 hand)

twoPair hand
	| (sameSizeCards $ take 2 hand) = (sameSizeCards $ take 2 $ drop 2 hand) ||
					  (sameSizeCards $ take 2 $ drop 3 hand)
	| (sameSizeCards $ take 2 $ drop 1 hand) = 
					  (sameSizeCards $ drop 3 hand)       
	| otherwise = False

onePair hand = 	(sameSizeCards $ take 2 hand) ||
		(sameSizeCards $ take 2 $ drop 1 hand) ||
		(sameSizeCards $ take 2 $ drop 2 hand) ||
		(sameSizeCards $ drop 3 hand)

--------------------------------------------------

straightFlushComp hand1 hand2
	| (size . head) hand1 > (size . head) hand2 = "hand 1"
	| otherwise = "hand 2"

foakComp hand1 hand2
	| cardSize hand1 > cardSize hand2 = "hand 1"
	| cardSize hand1 < cardSize hand2 = "hand 2"
	| topOfSort hand1 > topOfSort hand2 = "hand 1"
	| otherwise = "hand 2" 
	where 
	cardSize n = size $ n !! 2
	topOfSort n = (size . head . head) $ filter (\x -> length x <= 1) (groupBy (\x y -> size x == size y) n)

flushComp hand1 hand2 = highestComp hand1 hand2

straightComp hand1 hand2
	| sizen hand1 > sizen hand2 = "hand 1"
	| otherwise = "hand 2"
	where sizen n = size $ last n

toakComp hand1 hand2
	| thd hand1 > thd hand2 = "hand 1"
	| thd hand1 < thd hand2 = "hand 2"
	| otherwise = highestComp (getTwo hand1) (getTwo hand2)
	where 
	thd n = size $ n !! 2
	flatten [] = []
	flatten (x:xs) = x ++ flatten xs
	getTwo n = flatten $ filter (\x -> length x == 3) (groupBy (\x y -> size x == size y) n)

toak1 = [(Card 2 'H'), (Card 2 'S'), (Card 2 'D'), (Card 5 'C'), (Card 8 'H')] 
toak2 = [(Card 2 'H'), (Card 2 'S'), (Card 2 'D'), (Card 5 'C'), (Card 7 'H')] 
	
twoPairComp hand1 hand2 =
	| 
	where	
	pairs n = filter (\x -> length x == 2) (groupBy (\x y -> size x == size y) n)
	flatten [] = []
	flatten (x:xs) = x ++ flatten xs
	
highestComp [] [] = "neither"
highestComp hand1 hand2
	| sizen hand1 > sizen hand2 = "hand 1"
	| sizen hand1 < sizen hand2 = "hand 2"
	| otherwise = highestComp (init hand1) (init hand2)
	where
	sizen n = size $ last n
--------------------------------------------------

getVal hand  
	| royalFlush hand = (hand, 9)
	| straightFlush hand = (hand, 8)
	| foak hand = (hand, 7)
	| fullHouse hand = (hand, 6)
	| flush hand = (hand, 5)
	| straight hand = (hand, 4)
	| toak hand = (hand, 3)
	| twoPair hand = (hand, 2)
	| onePair hand = (hand, 1)
	| otherwise = (hand, 0)

compare hand1 hand2
	| val1 > val2 = "hand 1"
	| val1 < val2 = "hand 2"
	where 
	val1 = snd $ getVal hand1
	val2 = snd $ getVal hand2



main = print $ sortHand exHand
