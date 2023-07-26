# Haskell Solitaire

This is a self-playing implementation of a game of Solitaire in Haskell, using the Eight Off rules. Eight Off Solitaire is a game where the goal is to move all cards onto foundation piles, each of which is started with an ace and continued with cards of the same suit, in ascending order. Here's a brief overview of some of the main elements:

- Card: This is a tuple with two values: `Pip` (the rank of the card from Ace to King) and `Suit` (Hearts, Clubs, Spades, Diamonds).

- Deck: A list of Cards.

- Board: The game board, containing foundations (where the game ends when all cards are placed here), columns (piles of cards that can be moved around), and reserves (additional cards that can be used).

- The `show` function for `Board` makes it easy to visualize the state of the game at any point in the terminal.

- `shuffle`: This function randomizes the order of the deck based on a provided seed for consistent shuffling.

- `eoDeal` and `sDeal` functions are used to deal out the initial game setup for both Eight Off and Spider Solitaire games.

- `findTopmostAces`, `findTopmostSuccessors` and `findTopmostPredecessors` are helper functions to identify cards that can be moved to the foundation or to another column.

- `toFoundations` moves all valid cards to the foundations. It recursively calls `moveToFoundations` until there are no more cards that can be moved to the foundations.

- `findMoves` generates a list of all possible next game states.

- `chooseMove` picks the first valid move from the list of possible moves.

- `haveWon` checks if the game has been won by looking if all columns are empty and no cards are left in the reserves.

- `playSolitaire` drives the game by continuously choosing moves until no more moves are available or the game is won. It returns the score of the board at the end of the game, which is the total number of cards in the foundations.

Please note that this game plays itself without any user input once the game starts, as it automatically selects moves based on the current game state. Also note that it may not always win, as some games of solitaire cannot be solved, or the automatic selection of moves may not always choose the optimal strategy.
