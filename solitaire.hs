import System.Random
import Data.List

-- DEFINE INITIAL DATATYPES
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Ord, Enum, Show)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
type Card = (Suit, Pip)
type Deck = [Card]

-- IMPLEMENT BASIC FUNCTIONALITY
-- List all 52 cards in a pack
pack :: Deck
pack = [(suit, pip) | suit <- [Hearts .. Diamonds], pip <- [Ace .. King]]

-- Returns the successor of a card (successor of a king is an ace of the same suit)
sCard :: Card -> Card
sCard (s, King) = (s, Ace) 
sCard (s, p) = (s, succ p)

-- Returns the successor of a card (predecessor of an ace is a king of the same suit)
pCard :: Card -> Card
pCard (s, Ace) = (s, King)
pCard (s, p) = (s, pred p)

-- Checks if the given card is an ace
isAce :: Card -> Bool
--isAce (s, p) | p == Ace = True
--             | otherwise = False
isAce (s, p) = if p == Ace then True else False

-- Checks if the given card is a king
isKing :: Card -> Bool
--isKing(s, p) | p == King = True
--             | otherwise = False
isKing (s, p) = if p == King then True else False

-- Takes a deck and returns a shuffled deck of the same cards
shuffle :: Int -> Deck -> Deck
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffle n d = [x | (x, n) <- sortBy cmp (zip d ((randoms (mkStdGen n)) :: [Int]))]

-- DEFINE DATATYPES TO REPRESENT AN EIGHT-OFF BOARD


