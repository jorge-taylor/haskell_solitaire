-- An implementation of solitaire in Haskell for COM2108 Functional Programming , a 2nd year module.
import Data.List
import System.Random

-- Initial datatypes
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Ord, Enum, Show)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
type Card = (Suit, Pip)
type Deck = [Card]

-- Datatypes for Eight Off Board
type Foundations = [Deck]
type Columns = [Deck]
type Reserves = [Card]
data Board = Empty | EOBoard (Foundations, Columns, Reserves) deriving (Eq, Ord)

instance Show Board where
  show (EOBoard (f, c, r)) = "EOBoard\nFoundations  " ++ show f ++ "\nColumns\n" ++ showColumns c ++ "Reserves    " ++ show r
    where
      showColumns [] = ""
      showColumns (c : cs) = show c ++ "\n" ++ showColumns cs

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
remove card [] = []
remove card (element : deck)
  | card == element = deck
  | otherwise = element : remove card deck

-- Deals an eight off board given a seed
eoDeal :: Int -> Board
eoDeal seed = EOBoard (foundations, columns, reserves)
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
toFoundations :: Board -> Board
toFoundations board
  | anyValidMoves board = toFoundations (autoplay board)
  | otherwise = board

-- Check that there is a valid move given an eight off board
anyValidMoves :: Board -> Bool
anyValidMoves board = (not . null) (getValidAces board ++ getValidSuccessors board)

-- Moves all valid cards to foundations (copying them over to foundations and deleting them from their original location)
autoplay :: Board -> Board
autoplay (EOBoard (foundations, columns, reserves)) = foldr moveValidToFoundations (EOBoard (foundations, columns, reserves)) (moveable_aces ++ moveable_successors)
  where
    moveable_aces = getValidAces (EOBoard (foundations, columns, reserves))
    moveable_successors = getValidSuccessors (EOBoard (foundations, columns, reserves))

-- Get a list of all aces which are in the topmost columns or reserves which can be moved to foundations
getValidAces :: Board -> Deck
getValidAces (EOBoard (_, columns, reserves)) = filter isAce (map last columns ++ reserves)

-- Get a list of all successors which are in the topmost columns or reserves which can be moved to foundations
getValidSuccessors :: Board -> Deck
getValidSuccessors (EOBoard (foundations, columns, reserves)) = [c2 | c1 <- topmost_foundations, c2 <- topmost_possibles, sCard c1 == c2 && not (isKing c1)]
  where
    topmost_foundations = map last (filter (not . null) foundations)
    topmost_possibles = map last (filter (not . null) columns) ++ reserves

-- Moves valid cards to foundations
moveValidToFoundations :: Card -> Board -> Board
moveValidToFoundations card (EOBoard (foundations, columns, reserves)) = EOBoard (updated_foundations, updated_columns, updated_reserves)
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