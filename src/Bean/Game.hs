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
--------------------------------------------------------------------------------

import Bean.Types


{-| 
  Ex. 1: Write an expression representing the starting position for the game.

  [JUSTIFY]
-}
startingPos :: Board
startingPos = error "Not implemented"


{-|
  Ex. 2: Compute the balance of material, defined as difference between the 
  number of Red beans and the number of Blue beans. Make use of explicit 
  recursion and pattern matching in your definition.

  [JUSTIFY]
-}
balance :: Board -> Int
balance = error "Not implemented"


{-| 
  Ex. 3: Implement a 'Show' instance for 'Board'. 
  The rendered version should be exactly as it appears in the specification.
  
  Also implement an 'Eq' instance for 'Piece' and for 'PieceType'.
  You should NOT derive these automatically.

  [JUSTIFY]
-}
instance {-# OVERLAPS #-} Show Board where
  show = error "Not implemented"

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