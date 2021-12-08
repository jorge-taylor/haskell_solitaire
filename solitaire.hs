-- An implementation of solitaire in Haskell for COM2108 Functional Programming, a 2nd year module.
import System.Random
import Data.List
import Data.Maybe

-- Initial datatypes
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Ord, Enum, Show)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
type Card = (Pip, Suit)
type Deck = [Card]

-- Datatypes for Eight Off Board
type Foundations = [Deck]
type Columns = [Deck]
type Reserves = [Card]
data Board = Empty | EOBoard (Foundations, Columns, Reserves) | SBoard (Foundations, Columns, Stock) deriving (Eq, Ord)

-- Datatypes for Spider Board
type Stock = [Card]

-- The board constant as specified in part 1 step 3, mirroring appendix A
practiseBoard :: Board
practiseBoard = EOBoard(foundations, columns, reserves)
  where
    foundations = []
    columns = [[(Four,Spades),(King,Clubs),(Queen,Hearts),(Ace,Hearts),(Seven,Diamonds),(Ace,Clubs)],
               [(Seven,Hearts),(Six,Clubs),(Five,Clubs),(Three,Diamonds),(Queen,Spades),(Five,Diamonds)],
               [(Eight,Diamonds),(Five,Hearts),(Queen,Diamonds),(Seven,Spades),(Ten,Diamonds),(King,Hearts)],
               [(Queen,Clubs),(Ten,Clubs),(Eight,Spades),(Seven,Clubs),(Six,Hearts),(Jack,Spades)],
               [(Four,Clubs),(Jack,Hearts),(King,Diamonds),(Ace,Diamonds),(Eight,Clubs),(Ace,Spades)],
               [(Jack,Clubs),(Six,Diamonds),(Ten,Hearts),(Three,Clubs),(Three,Hearts),(Two,Diamonds)],
               [(Ten,Spades),(Three,Spades),(Nine,Hearts),(Nine,Clubs),(Four,Diamonds),(Nine,Spades)],
               [(Eight,Hearts),(King,Spades),(Nine,Diamonds),(Four,Hearts),(Two,Spades),(Two,Clubs)]]
    reserves = [(Two,Hearts), (Six,Clubs),(Five,Clubs),(Jack,Diamonds)]

-- Creates an instance of Board with a specific way of showing boards so it is more legible in the terminal
instance Show Board where
  show (EOBoard (f, c, r)) = "\nEOBoard\nFoundations\n" ++ showFoundations f ++ "\nColumns\n" ++ showColumns c ++ "Reserves\n" ++ show r ++ "\n"
    where
      showFoundations f = show (map last f)
      showColumns [] = ""
      showColumns (c : cs) = show c ++ "\n" ++ showColumns cs
  show (SBoard (f, c, s)) = "SBoard\nFoundations  " ++ show f ++ "\nColumns\n" ++ showColumns c ++ "Stock  " ++ show (length s) ++ " Deals remaining"
    where
      showColumns [] = ""
      showColumns (c : cs) = show c ++ "\n" ++ showColumns cs

-- List all 52 cards in a pack
pack :: Deck
pack = [(pip, suit) | pip <- [Ace .. King], suit <- [Hearts .. Diamonds]]

-- Returns the successor of a card (successor of a king is an ace of the same suit)
sCard :: Card -> Card
sCard (King, suit) = (Ace, suit)
sCard (pip, suit) = (succ pip, suit)

-- Returns the successor of a card (predecessor of an ace is a king of the same suit)
pCard :: Card -> Card
pCard (Ace, suit) = (King, suit)
pCard (pip, suit) = (pred pip, suit)

-- Checks if the given card is an ace
isAce :: Card -> Bool
isAce (pip, _) = pip == Ace

-- Checks if the given card is a king
isKing :: Card -> Bool
isKing (pip, _) = pip == King

-- Checks if the given cards are the same suit
sameSuit :: Card -> Card -> Bool
sameSuit (_, suit1) (_, suit2) = suit1 == suit2

-- Takes a deck and an int and returns a shuffled deck of the same cards
shuffle :: Deck -> Int -> Deck
cmp (x1, y1) (x2, y2) = compare y1 y2
shuffle deck seed = [x | (x, n) <- sortBy cmp (zip deck (randoms (mkStdGen seed) :: [Int]))] -- zip random values to the deck then sort them

-- Deals an eight off board given a seed
eoDeal :: Int -> Board
eoDeal seed = EOBoard (foundations, columns, reserves)
  where
    deck = shuffle pack seed
    foundations = []
    columns = eoSplitColumns (drop 4 deck)
    reserves = take 4 deck

-- Deals a spider board given a seed
sDeal :: Int -> Board
sDeal seed = SBoard (foundations, columns, stock)
  where
    deck = shuffle pack seed ++ shuffle pack seed
    foundations = []
    columns = sSplitColumns (drop 50 deck)
    stock = take 50 deck

-- Splits a deck into 8 piles of 6 cards
eoSplitColumns :: Deck -> [Deck]
eoSplitColumns [] = []
eoSplitColumns deck = head : eoSplitColumns tail
  where
    (head, tail) = splitAt 6 deck

-- Splits a deck into 6 piles of 5 and the rest in piles of 6
sSplitColumns :: Deck -> [Deck]
sSplitColumns [] = []
sSplitColumns deck = head : sSplitColumns tail
  where
    (head, tail) = splitAt 6 deck
    {--
    if length tail > 30
      then (head, tail) = splitAt 6 deck
      else (head, tail) = splitAt 5 deck
      --}

-- Initial function which calls toFoundations
toFoundations :: Board -> Board
toFoundations board
  | anyToFoundationMoves board = toFoundations (moveToFoundations board)
  | otherwise = board

-- Moves all valid cards to foundations (copying them over to foundations and deleting them from their original location)
moveToFoundations :: Board -> Board
moveToFoundations (EOBoard (foundations, columns, reserves)) = foldr cardToFoundations (EOBoard (foundations, columns, reserves)) (moveable_aces ++ moveable_successors)
  where
    moveable_aces = findTopmostAces (EOBoard (foundations, columns, reserves))
    moveable_successors = findTopmostSuccessors (EOBoard (foundations, columns, reserves))

-- Check that there is a valid move given an eight off board
anyToFoundationMoves :: Board -> Bool
anyToFoundationMoves board = (not . null) (findTopmostAces board ++ findTopmostSuccessors board)

-- Get a list of all aces which are in the topmost columns or reserves which can be moved to foundations
findTopmostAces :: Board -> Deck
findTopmostAces (EOBoard (_, columns, reserves)) = filter isAce (map last (filter (not . null) columns) ++ reserves)

-- Get a list of all successors which are in the topmost columns or reserves which can be moved to foundations
findTopmostSuccessors :: Board -> Deck
findTopmostSuccessors (EOBoard (foundations, columns, reserves)) = [c2 | c1 <- topmost_foundations, c2 <- topmost_possibles, sCard c1 == c2 && not (isKing c1)]
  where
    topmost_foundations = map last (filter (not . null) foundations)
    topmost_possibles = map last (filter (not . null) columns) ++ reserves

-- Get a list of all predecessors which are in the topmost columns or reserves which can be moved to the columns
findTopmostPredecessors :: Board -> Deck
findTopmostPredecessors (EOBoard (foundations, columns, reserves)) = [c2 | c1 <- topmost_columns, c2 <- topmost_possibles, pCard c1 == c2 || isKing c2 && elem [] columns]
  where
    topmost_columns = map last (filter (not . null) columns)
    topmost_possibles = map last (filter (not . null) columns) ++ reserves

-- Moves valid cards to foundations deleting them from their old place
cardToFoundations :: Card -> Board -> Board
cardToFoundations card (EOBoard (foundations, columns, reserves)) = EOBoard (new_foundations, new_columns, new_reserves)
  where
    new_foundations = insertCardToFoundations card foundations
    new_columns = map (delete card) columns
    new_reserves = delete card reserves

-- Takes a card and decides the appropriate action when it moves it to foundations based on whether the card is an ace or a successor
insertCardToFoundations :: Card -> Foundations -> Foundations
insertCardToFoundations card foundations
  | isAce card = [card] : foundations
  | otherwise = map (\e -> if (not.null) e && sameSuit card (last e) then reverse (card : e) else e) foundations

-- Find all valid moves not to foundations, creating a list of boards in order of the best moves
findMoves :: Board -> [Board]
findMoves board@(EOBoard (foundations, columns, reserves)) = moveToColumnsBoard ++ moveToReservesBoard
  where
    topmost_predecessors = findTopmostPredecessors board
    topmost_columns = map last (filter (not.null) columns)

    moveToColumnsBoard = [toFoundations (moveToColumns tp board) | tp <- topmost_predecessors]
    moveToReservesBoard = [toFoundations (cardToReserves tc board) | tc <- topmost_columns]

-- Helper function for moving a card to a column, it deletes that card from its previous position
moveToColumns :: Card -> Board -> Board
moveToColumns card (EOBoard (foundations, columns, reserves)) = EOBoard(foundations, new_columns, new_reserves)
  where
    new_columns = cardToColumns card (map (delete card) columns)
    new_reserves = delete card reserves

-- Takes a card and inserts the card into the correct column
cardToColumns :: Card -> Columns -> Columns
cardToColumns card [] = []
cardToColumns card (c:columns)
  | isKing card && null c = (c ++ [card]) : columns
  | (not . null) c && sCard card == last c = (c ++ [card]) : columns
  | otherwise = c:cardToColumns card columns

-- Takes a card and inserts it into reserves and deletes where it previously was
cardToReserves :: Card -> Board -> Board
cardToReserves card (EOBoard (foundations, columns, reserves)) = EOBoard (foundations, new_columns, new_reserves)
  where
    new_columns = if length reserves >= 8 then columns else map (delete card) columns
    new_reserves = if length reserves >= 8 then reserves else reserves ++ [card]

-- Chooses the first move from findMoves which returns a list of boards with the best moves at the front
chooseMove :: Board -> Maybe Board
chooseMove board
    | (not . null) (findMoves board) = Just (head (findMoves board))
    | otherwise = Nothing

-- Checks if the current board is in a winning state
haveWon :: Board -> Bool
haveWon (EOBoard (_, columns, reserves)) = columnsEmpty columns && null reserves

-- Checks that all columns are empty
columnsEmpty :: Columns -> Bool
columnsEmpty [] = True
columnsEmpty (c : columns)
  | null c = columnsEmpty columns
  | otherwise = False

-- Checks how many cards are in the foundations (the score of the board)
score :: Board -> Int
score (EOBoard (foundations, _, _)) = sum (map length foundations)

-- Plays 8-off solitaire by calling choose move until either there are no moves or the board is in a winning state
playSolitaire :: Board -> Int
playSolitaire board@(EOBoard(columns, reserves, foundations))
    | haveWon board = score board
    | isJust (chooseMove board) = playSolitaire (fromJust $ chooseMove board)
    | otherwise = score board

-- template.hs 
{- Paste the contents of this file, including this comment, into your source file, below all
    of your code. You can change the indentation to align with your own, but other than this,
    ONLY make changes as instructed in the comments.
  -}
-- Constants that YOU must set:
studentName = "Jorge Taylor"
studentNumber = "200130305"
studentUsername = "acb20jt"

initialBoardDefined = practiseBoard {- replace XXX with the name of the constant that you defined
                              in step 3 of part 1 -}
secondBoardDefined = practiseBoard {- replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
  do
    putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

    putStrLn "***The eight-off initial board constant from part 1:"
    print initialBoardDefined

    let board = toFoundations initialBoardDefined
    putStrLn "***The result of calling toFoundations on that board:"
    print board

    {- Move the start comment marker below to the appropriate position.
      If you have completed ALL the tasks for the assignment, you can
      remove the comments from the main function entirely.
      DO NOT try to submit/run non-functional code - you will receive 0 marks
      for ALL your code if you do, even if *some* of your code is correct.
    -}

    let boards = findMoves board      -- show that findMoves is working
    putStrLn "***The possible next moves after that:"
    print boards

    let chosen = chooseMove board     -- show that chooseMove is working
    putStrLn "***The chosen move from that set:"
    print chosen

    putStrLn "***Now showing a full game"     -- display a full game
    score <- displayGame initialBoardDefined 0
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)

    {- start comment marker - move this if appropriate

    putStrLn "\n\n\n************\nNow looking at the alternative game:"

    putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
    print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                      -- is not an initial game, but a point from which the game
                                      -- can be won

    putStrLn "***Now showing a full game for alternative solitaire"
    score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                              -- works correctly)
    putStrLn $ "Score: " ++ score
    putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

    -}

{- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}

displayGame :: Board -> Int ->IO String
displayGame board n =
  if haveWon board
    then return "A WIN"
    else
      do
        putStr ("Move " ++ show n ++ ": " ++ show board)
        let maybeBoard = chooseMove board
        if isJust maybeBoard then
          do
            let (Just newBoard) = maybeBoard
            displayGame newBoard (n+1)
        else
          do
            let score = show (playSolitaire board)
            return score
