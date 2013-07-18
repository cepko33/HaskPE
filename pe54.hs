import Data.List (sortBy, all)
import Data.Ord (comparing)

-- a Card is the Size then the Suit, e.g. (Card '5' 's')
data Card = Card Int Char deriving (Show)

size (Card csize _) = csize
suit (Card _ csuit) = csuit

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

compPair hand1 hand2
	|

main = print $ sortHand exHand
