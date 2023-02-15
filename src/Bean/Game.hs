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

-- Got idea to use intercalate function for Ex3 https://stackoverflow.com/questions/13846870/using-show-with-a-list-of-lists-in-haskell
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

  [JUSTIFY]
  I made use of the intercalate function from the the Data.List module function in order to add the 
  \n character between rows of the board for the show function (so each row gets printed to a new line). 
  This was needed to avoid the addition of a newline at the end - which
  happened in my first attempt using '++' to add a newline after every row.
  I decided to use list comprehensions here instead of the 'map' function, as this implementation
  is much more readable than the alternative of 'show b = intercalate "\n" (map (\r -> concat(map (\p -> show p) r)) b)' 
  in my opinion.

  For the Eq instances of Piece and PieceType, I just manually defined the cases where the two values were equal
  (and the '==' operator should return True), and used the '_' wildcard keyword to pattern match all other possible combinations as 
  False, to avoid having to write all the other instances which should evalaute to False.
-}

instance {-# OVERLAPS #-} Show Board where
  show b = intercalate "\n" [ (concat [ show p | p <- r ]) | r <- b ]


instance Eq PieceType where
  Cow == Cow = True
  Bean == Bean = True
  _ == _ = False

instance Eq Piece where
  Red Cow == Red Cow = True
  Red Bean == Red Bean = True
  Blue Cow == Blue Cow = True
  Blue Bean == Blue Bean = True
  Empty == Empty = True
  _ == _ = False

{-| 
  Ex. 4: Implement a function that gets a piece at a coordinate. 
  If the location is off the board, return Nothing, otherwise return the 
  appropriate piece (or Empty), wrapped in a Just.

  [JUSTIFY]
  I used guards here to return the appropiate output - pattern matching/case statements wouldn't
  have worked as we need to check a predicate of the input, not for specific inputs, and I 
  think guards are easier to read than if/else statements in Haskell. 
  
  To check for the piece in a specific Co-ordinate, I ended up using the !! in the Data.List module.
  The drawback of the operator being a partial function won't be an issue here, as if the co-ords
  are out of scope, the function will have returned Nothing earlier - meaning the !! operator will
  be called for only values it is defined for (values that aren't bigger than the size of the
  board).
  
-}
getPiece :: Board -> Coord -> Maybe Piece
getPiece b (x, y) 
  | x < 0 || x > 3 || y < 0 || y > 3         = Nothing
  | otherwise                                = Just ((b !! y) !! x)


{-| 
  Ex. 5: Return the valid moves that a cow in position (x,y) would have.

  [JUSTIFY]
  I used a list comprehension to build a list of valid cow moves, with it
  taking moves from a list defined by calling a new 'possibleMoves' function, but only
  if the move would move the cow into a blank space.

  I could have used a filter function here - e.g. 'validCowMoves b (x, y) = filter (\(x, y) -> getPiece b (x, y) == Just Empty) (possibleMoves (x, y))' -
  but I found the list comprhension was clearer to read in my opinion.
-}

possibleMoves :: Coord -> [Coord]
possibleMoves (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

validCowMoves :: Board -> Coord -> [Coord]
validCowMoves b (x, y) = [(x', y') | (x', y') <- possibleMoves (x, y),  getPiece b (x', y') == Just Empty]


{-| 
  Ex. 6: Return the valid moves for a bean on the given team in position c.

  [JUSTIFY]
-}



beanMoveIsValid :: Maybe Piece -> Player -> Bool
beanMoveIsValid m p
  | m == Just Empty                           = True
  | p == RedPlayer && m == Just (Blue Bean)     = True
  | p == BluePlayer && m == Just (Red Bean)     = True
  | otherwise                                 = False

validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves b p (x, y) = [(x', y') | (x', y') <- possibleMoves (x, y), beanMoveIsValid ( getPiece b (x', y')) p]


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