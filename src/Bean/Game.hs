module Bean.Game where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.

-- Got idea to use inetercalate function for Ex3 https://stackoverflow.com/questions/13846870/using-show-with-a-list-of-lists-in-haskell
--------------------------------------------------------------------------------

import Bean.Types
import Data.List


{-| 
  Ex. 1: Write an expression representing the starting position for the game.

  [JUSTIFY]
  I decided to write the lists using the 'syntactic sugar' method, rather than
  using the cons operator, as this makes the code much more readable, without sacrificing speed,
  because the syntatic sugar will be converted into cons operators at compile time.

-}
startingPos :: Board
startingPos = 
  [[Empty, Red Cow, Red Cow, Empty],
   [Red Bean, Red Bean, Red Bean, Red Bean],
   [Blue Bean, Blue Bean, Blue Bean, Blue Bean],
   [Empty, Blue Cow, Blue Cow, Empty]
  ]


{-|
  Ex. 2: Compute the balance of material, defined as difference between the 
  number of Red beans and the number of Blue beans. Make use of explicit 
  recursion and pattern matching in your definition.

  [JUSTIFY]

  RED - BLUE
-}
balance :: Board -> Int
balance = error "Not implemented"


{-| 
  Ex. 3: Implement a 'Show' instance for 'Board'. 
  The rendered version should be exactly as it appears in the specification.
  
  Also implement an 'Eq' instance for 'Piece' and for 'PieceType'.
  You should NOT derive these automatically. 

  TODO: USE GHCI or WHATEVER TO SEE WHAT IS HAPPENING

  [JUSTIFY]
-}
instance {-# OVERLAPS #-} Show Board where
  {-show b = map (\p -> show p) (concat (intersperse ["\n"] b))-}
  {-show b = map (\p -> (show p :: Piece)) (concat b)-}
  show :: Board -> String
  {-show b = show (concat b)- WORKS-}
  {-show b = concat [(concat [show p | p <- r]) ++ "\n" | r <- b]-} 
  show b = concat (intersperse "\n" [(concat [show p | p <- r]) | r <- b])

  

  

instance Eq PieceType where
  (==) = error "Not implemented"

instance Eq Piece where
  (==) = error "Not implemented"


{-| 
  Ex. 4: Implement a function that gets a piece at a coordinate. 
  If the location is off the board, return Nothing, otherwise return the 
  appropriate piece (or Empty), wrapped in a Just.

  [JUSTIFY]
-}
getPiece :: Board -> Coord -> Maybe Piece
getPiece = error "Not implemented"


{-| 
  Ex. 5: Return the valid moves that a cow in position (x,y) would have.

  [JUSTIFY]
-}
validCowMoves :: Board -> Coord -> [Coord]
validCowMoves = error "Not implemented"


{-| 
  Ex. 6: Return the valid moves for a bean on the given team in position c.

  [JUSTIFY]
-}
validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves = error "Not implemented"


{-| 
  Ex. 7: Set a given (valid) coordinate to have a given piece (or Empty).

  [JUSTIFY]
-}
setCoord :: Board -> Coord -> Piece -> Board
setCoord = error "Not implemented"


{-| 
  Ex. 8: Given two positions, return Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, return Nothing instead.

  [JUSTIFY]
-}
makeMove :: Board -> Coord -> Coord -> Maybe Board
makeMove = error "Not implemented"


{-| 
  Ex. 9: The game is drawn if the same setup is repeated, or after the fiftieth 
  move. Given a sequence of boards, most recent first, determine whether the 
  game is drawn in any position.

  [JUSTIFY]
-}
gameIsDrawn :: [Board] -> Bool
gameIsDrawn = error "Not implemented"


{-| 
  Ex. 10: The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determine whether the game is won by the opponent.

  [JUSTIFY]
-}
gameIsWon :: Board -> Player -> Bool
gameIsWon = error "Not implemented"


{-| 
  Finale: Implement a computer player to play the game of Bean's Gambit.
  Given the history of the game as a (nonempty) list, the AI should return an 
  appropriate next move as a pair (from, to).

  [JUSTIFY]
-}
nextMove :: [Board] -> Player -> (Coord, Coord)
nextMove = error "Not implemented"