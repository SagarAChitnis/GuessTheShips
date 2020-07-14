# GuessTheShips
This is a two player game coded in Haskell

This is a simple two-player logical guessing game created for this project. 
The game is somewhat akin to the game of Battleship™, but somewhat simplified. The game is played on a 4×8 grid, and involves one player, the searcher trying to find the locations of three battleships hidden by the other player, the hider. The searcher continues to guess until they find all the hidden ships. Unlike Battleship™, a guess consists of three different locations, and the game continues until the exact locations of the three hidden ships are guessed in a single guess. After each guess, the hider responds with three numbers:
1.	the number of ships exactly located;
2.	the number of guesses that were exactly one space away from a ship; and
3.	the number of guesses that were exactly two spaces away from a ship.

Each guess is only counted as its closest distance to any ship. For example if a guessed location is exactly the location of one ship and is one square away from another, it counts as exactly locating a ship, and not as one away from a ship. The eight squares adjacent to a square, including diagonally adjacent, are counted as distance 1 away. The sixteen squares adjacent to those squares are considered to be distance 2 away, as illustrated in this diagram of distances from the center square:



Of course, depending on the location of the center square, some of these locations will actually be outside the board.
Note that this feedback does not tell you which of the guessed locations is close to a ship. Your program will have to work that out; that is the challenge of this project.
We use a chess-like notation for describing locations: a letter A–H denoting the column of the guess and a digit 1–4 denoting the row, in that order. The upper left location is A1 and the lower right is H4.
A few caveats:
•	The three ships will be at three different locations.
•	Your guess must consist of exactly three different locations.
•	Your list of locations may be written in any order, but the order is not significant; the guess A3, D1, H1 is exactly the same as H1, A3, D1 or any other permutation.
Here are some example ship locations, guesses, and the feedback provided by the hider:
           
Locations	  |    Guess      | 	Feedback
H1, B2, D3	|  B3, C3, H3  	|   0, 2, 1
H1, B2, D3	|  B1, A2, H3	  |   0, 2, 1
H1, B2, D3	|  B2, H2, H1	  |   2, 1, 0
A1, D2, B3	|  A3, D2, H1	  |   1, 1, 0
A1, D2, B3	|  H4, G3, H2	  |   0, 0, 0
A1, D2, B3	|  D2, B3, A1	  |   3, 0, 0

Here is a graphical depiction of the first example above, where ships are shown as S and guessed locations are shown as G:


The game finishes once the searcher guesses all three ship locations in a single guess (in any order), such as in the last example above. The object of the game for the searcher is to find the target with the fewest possible guesses.

