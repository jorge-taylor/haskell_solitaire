import System.Random
import Data.List

-- Initial datatypes
data Suit = Hearts | Clubs | Spades | Diamonds
  deriving (Eq, Ord, Enum, Show)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Ord, Enum, Show)
type Card = (Suit, Pip)
type Deck = [Card]

-- Datatypes for Eight Off Board
type Foundations = [Deck]
type Columns = [Deck]
type Reserves = [Card]
data Board = Empty | EOBoard deriving (Eq, Ord, Enum, Show)
type EOBoard = (Foundations, Columns, Reserves)
{--
instance Show EOBoard where
  Foundations = "Foundations" + Foundations
  Columns = "Columns" + Columns
  Reserves = "Reserves" + Reserves
--}

-- List all 52 cards in a pack
pack :: Deck
pack = [(suit, pip) | suit <- [Hearts .. Diamonds], pip <- [Ace .. King]]

-- Returns the successor of a card (successor of a king is an ace of the same suit)
sCard :: Card -> Card
sCard (suit, King) = (suit, Ace) 
sCard (suit, pip) = (suit, succ pip)

-- Returns the successor of a card (predecessor of an ace is a king of the same suit)
pCard :: Card -> Card
pCard (suit, Ace) = (suit, King)
pCard (suit, pip) = (suit, pred pip)

-- Checks if the given card is an ace
isAce :: Card -> Bool
isAce (_, pip) = pip == Ace

-- Checks if the given card is a king
isKing :: Card -> Bool
isKing(_, pip) = pip == King

-- Takes a deck and returns a shuffled deck of the same cards
shuffle :: Int -> Deck
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffle seed = [x | (x, n) <- sortBy cmp (zip pack ((randoms (mkStdGen seed)) :: [Int]))]

-- Deals an eight off board given a seed 
eoDeal :: Int -> EOBoard
eoDeal seed = (foundations, columns, reserves)
  where deck = shuffle seed
        foundations = []
        reserves = take 4 deck
        columns = splitDeck (drop 4 deck)

-- Splits a deck recursively into piles of 6, to create 8 columns 
splitDeck :: Deck -> [Deck]
splitDeck [] = []
splitDeck deck = head : splitDeck tail
    where (head, tail) = splitAt 6 deck