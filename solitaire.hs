{--
An implementation of solitaire in Haskell for COM2108 Functional Programming , a 2nd year module.
Author: Jorge Taylor
Created: 02/11/2021
Last modified: 14/11/2021
--}

import Data.List
import System.Random

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

data Board = Empty | EOBoard
  deriving (Eq, Ord, Enum, Show)

type EOBoard = (Foundations, Columns, Reserves)

-- output an EOBoard in the desired format
-- problem: this is not the correct way to implement this, what about: "instance Show EOBoard where"?
outputEO :: EOBoard -> IO ()
outputEO board@(f, c, r) = do
  putStr "EOBoard\nFoundations "
  putStrLn (show f)
  putStr "Columns\n"
  putStrLn (show c)
  putStr "Reserve     "
  putStrLn (show r)

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
isKing (_, pip) = pip == King

-- Takes a deck and returns a shuffled deck of the same cards
shuffle :: Int -> Deck

cmp (x1, y1) (x2, y2) = compare y1 y2

shuffle seed = [x | (x, n) <- sortBy cmp (zip pack (randoms (mkStdGen seed) :: [Int]))]

-- Removes the first instance of a card from a given list of cards (deck)
remove :: Card -> Deck -> Deck
remove card (element : deck)
  | card == element = deck
  | otherwise = element : remove card deck

-- Deals an eight off board given a seed
eoDeal :: Int -> EOBoard
eoDeal seed = (foundations, columns, reserves)
  where
    deck = shuffle seed
    foundations = []
    columns = splitColumns (drop 4 deck)
    reserves = take 4 deck

-- Splits a deck recursively into piles of 6, to create 8 columns
splitColumns :: Deck -> [Deck]
splitColumns [] = []
splitColumns deck = head : splitColumns tail
  where
    (head, tail) = splitAt 6 deck

-- Calls recursively autoplay while there is a valid move to foundations
toFoundations :: EOBoard -> EOBoard
toFoundations board
  | anyValidMoves board = toFoundations (autoplay board)
  | otherwise = board

-- Check that there is a valid move given an eight off board
anyValidMoves :: EOBoard -> Bool
anyValidMoves board = (not . null) (getValidAces board ++ getValidSuccessors board)

-- Moves all valid cards to foundations (copying them over to foundations and deleting them from their original location)
autoplay :: EOBoard -> EOBoard
autoplay board@(foundations, columns, reserves) = (updated_foundations, updated_columns, updated_reserves)
  where
    moveable_aces = getValidAces board
    moveable_successors = getValidSuccessors board
    --possibles_in_columns =
    --possibles_in_reserves =

    (updated_foundations, _, _) = foldr moveValidsToFoundations board (moveable_aces ++ moveable_successors)
    updated_columns = filter (not . null) (map (\e -> if isAce (last e) || any (elem (last e)) [moveable_successors] then init e else e) columns)
    --updated_columns = [element | element <- possibles_in_columns]
    updated_reserves = filter (\e -> not (isAce e || e `elem` moveable_successors)) reserves

-- Get a list of all aces which are in the topmost columns or reserves which can be moved to foundations
getValidAces :: EOBoard -> Deck
getValidAces (_, columns, reserves) = filter isAce (map last columns ++ reserves)

-- Get a list of all successors which are in the topmost columns or reserves which can be moved to foundations
getValidSuccessors :: EOBoard -> Deck
--filter (\e -> e `elem` (map head (filter (not.null) columns) ++ reserves)) (map (\e -> if isKing (head e) then head e else (sCard.head) e) foundations)
getValidSuccessors (foundations, columns, reserves) = [c2 | c1 <- topmost_foundations, c2 <- topmost_possibles, sCard c1 == c2 && not (isKing c1)]
  where
    topmost_foundations = map last (filter (not . null) foundations)
    topmost_possibles = map last (filter (not . null) columns) ++ reserves

-- Moves valid cards to foundations
moveValidsToFoundations :: Card -> EOBoard -> EOBoard
moveValidsToFoundations card (foundations, columns, reserves) = (updated_foundations, updated_columns, updated_reserves)
  where
    updated_foundations = cardToFoundations card foundations
    updated_columns = map (remove card) columns
    updated_reserves = remove card reserves

-- Takes a card decides the appropriate action when it moves it to foundations based on whether the card is an ace or a successor
cardToFoundations :: Card -> Foundations -> Foundations
cardToFoundations card foundations
  | isAce card = [card] : foundations
  | otherwise = map (\e -> if sameSuit card (last e) then reverse (card : e) else e) foundations

-- Checks whether two cards are of the same suit
sameSuit :: Card -> Card -> Bool
sameSuit (suit1, _) (suit2, _) = suit1 == suit2