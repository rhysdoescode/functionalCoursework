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
-- Got idea to use 'take' and 'drop' functions to make a new list with one element changed for Ex7: https://stackoverflow.com/questions/15530511/how-to-set-value-in-nth-element-in-a-haskell-list
-- Learned how to use 'maybe' funcion to pull out a value from a maybe return type for Ex8: https://stackoverflow.com/questions/28706843/getting-a-value-from-maybe-a-return-type-in-haskell
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
  These two functions  needed to iterates through every Piece in the list - 
  if the piece is a Red Bean, the balance is incremented; if the piece is a blue bean,
  the balance is decremented - once every item in the list is evaluated, the end result returned
  is the same as the difference between all the red peices and blue peices.

  Because he 'balance' function only took in a Board parameter, which would mean revursive steps wouldn't be able to access a 'balance' variable
  from the previous step - I made a new function called countBeans, which took in and returned a pair - 
  a concatinated version of the board - to make iteration through the list easier - and the running balance total.

  Pattern matching is used to update the balance varible differently depending on what the piece is - I could have
  used 'case' statements, but this looks more readable in my opinion - and to check for the 'base case', when the
  list is empty. Each recursive call to countBeans is on a smaller and smaller list, so the function will
  always eventually become empty. Therefore, it will always return to the base case in a finite amount of time.

  I made use of the 'snd' function to access the second value of the returned tuple - so the balance function
  would return only the balance integer, as required.

-}
balance :: Board -> Int
balance b = snd (countBeans ((concat b), 0))

countBeans :: ([Piece], Int) -> ([Piece], Int)
countBeans ((Red Bean : xs), b) = countBeans (xs, b + 1)
countBeans ((Blue Bean : xs), b) = countBeans (xs, b - 1)
countBeans ( (_ : xs), b) =  countBeans (xs, b)
countBeans ([], b) = ([], b)


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
  
  To check for the piece in a specific Co-ordinate, I ended up using the !! in the Data.List module,
  which was neccessary because this program involves lots of working with lists (e.g. the whole
  board is a list), so getting access to functions which work with lists is very useful.

  The drawback of the operator '!!' being a partial function won't be an issue here, as if the co-ords
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
  Similar to the previous exercise, I used a list comprehension to make the list of validBeanMoves.
  I decided to split off the functionality of the guard into its own function (beanMoveIsValid), which uses
  guards to determine if the move is acutally valid, before return a boolean result -
  to avoid the problem of having one very long line of code inside the list comprehension. 

-}

beanMoveIsValid :: Maybe Piece -> Player -> Bool
beanMoveIsValid m p
  | m == Just Empty                             = True
  | p == RedPlayer && m == Just (Blue Bean)     = True
  | p == BluePlayer && m == Just (Red Bean)     = True
  | otherwise                                   = False

validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves b p (x, y) = [(x', y') | (x', y') <- possibleMoves (x, y), beanMoveIsValid (getPiece b (x', y')) p]


{-| 
  Ex. 7: Set a given (valid) coordinate to have a given piece (or Empty).

  [JUSTIFY]
  As in haskell lists are immutable, I needed to create an entirely new board, with the co-ord
  specified changed. To do this, I used the 'take' and 'drop' methods to extract the rows that aren't being
  changed, and then made a new function 'changeRow' that changes the specified row - before combining the 
  rows into one list using the '++' operator.

  I used the '!!' despite it being partial, because the exercise text above specifies that the given 
  co-ordinate is valid, meaning the operator won't be given any values it isn't defined for.
-}

changeRow :: Int -> [Piece] -> Piece -> [[Piece]]
changeRow x r p = [(take x r) ++ [p] ++ (drop (x+1) r)]

setCoord :: Board -> Coord -> Piece -> Board
setCoord b (x, y) p = take y b ++ changeRow x (b !! y) p ++ drop (y+1) b


{-| 
  Ex. 8: Given two positions, return Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, return Nothing instead.

  [JUSTIFY]
  For this function, I decided to use guards for deciding between the different return values, which is 
  most appropiate as we are checking the lots of predicates (e.g. if the statement 'this peice is a blue bean, and
  the proposed move is a valid move' is true). I could have used if statements here but I think guards are easier to read.

  I split off some parts into their own functions, to make the code more readable - I made a 'movePiece' function
  which updates the board, calling the setCoord function once to make the old location of the piece empty, and then again
  on using the returned board as a parameter from the previous setCoord call to update the location of the moved peice. 

  I made use of the 'elem' function, to check if the given move is in the list of valid moves generated by the
  validBeanMoves/validCowMoves functions - which the Haskell documentations says is often used 'infix' form, so I decided to
  use it that way to increase readability.

  I made use of the 'where' keyword here to store a local variable 'p' - this will avoid repeated calls to getPiece, which will
  make the code run faster (and also make it more concise).
-}

pieceIsCow :: Maybe Piece -> Bool
pieceIsCow p = p == Just (Red Cow) || p == Just (Blue Cow)

movePiece :: Board -> Coord -> Coord -> Maybe Piece -> Maybe Board
movePiece b (x1, y1) (x2, y2) p = Just (setCoord (setCoord b (x1, y1) Empty) (x2, y2) (maybe Empty id p))

makeMove :: Board -> Coord -> Coord -> Maybe Board
makeMove b (x1, y1) (x2, y2) 
  | p == Just (Red Bean) && (x2, y2) `elem` validBeanMoves b RedPlayer (x1, y1)      = movePiece b (x1, y1) (x2, y2) p
  | p == Just (Blue Bean) && (x2, y2) `elem` validBeanMoves b BluePlayer (x1, y1)    = movePiece b (x1, y1) (x2, y2) p
  | pieceIsCow p && (x2, y2) `elem` validCowMoves b (x1, y1)                         = movePiece b (x1, y1) (x2, y2) p
  | otherwise                                                                        = Nothing
  where p = getPiece b (x1, y1) 
        

{-| 
  Ex. 9: The game is drawn if the same setup is repeated, or after the fiftieth 
  move. Given a sequence of boards, most recent first, determine whether the 
  game is drawn in any position.

  [JUSTIFY]
  To make the gameIsDraw function, I ended up using guards, as it is the most appropiate as
  we are checking predicates. The length predicate was simple, although I needed to take into 
  account that before the first move one board already exists in the array. 

  To check if the board had any repeated positions, I made a 'repeatedPosition' function, which
  in effect uses recursion to check the list one element at a time to see if any elements repeat twice, starting
  from the first item in the list - if one is found, it returns true, but if the whole list is searched and no repeating baords are found, it reutrns false. 
  The function makes use of the 'elem' function to see if another instance of the same board exists in the list after the first instance of the board is removed.

  The function is fairly efficient, as it immediatly returns a True value when a repeating board is
  found - while other implementations may always check the entire list before returning a value. (Although
  it still has O(n^2) time complexity because the whole list is checked in the worst case scenario - if no
  repeating boards exist).
-}

repeatedPosition :: [Board] -> Bool
repeatedPosition b 
  | length b > 1          = if ((head b) `elem` (tail b)) 
                              then True
                            else repeatedPosition (tail b)
  | otherwise             = False

gameIsDrawn :: [Board] -> Bool
gameIsDrawn b 
  | length b >= 51        = True
  | repeatedPosition b    = True
  | otherwise             = False


{-| 
  Ex. 10: The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determine whether the game is won by the opponent.

  [JUSTIFY]
  For the gameIsWon function, I used list comprehensions, as this seemed like the best way
  to iterate through all the possible x and y values of the board, which were needed for the 
  various functions to test if the peices iterated through were cows, and if so, if they were blocked.

  I make seperate 'pieceIsBlueBlockedCow' and 'pieceIsRedBlockedCow' functions to make the list comprehensions
  less long, and therefore more readable.

  The or function at the end returns True if any of the values in the generated list are true i.e. if any of the pieces
  on the board are blocked cows of the specified color. This function is a little inefficient as the list will keep
  generating even after a blocked cow is found, and could be improved in future iterations.

-}

pieceIsBlueBlockedCow :: Board -> Coord -> Bool
pieceIsBlueBlockedCow b (x, y) = getPiece b (x, y) == Just (Blue Cow) && length (validCowMoves b (x, y)) == 0
  
pieceIsRedBlockedCow :: Board -> Coord -> Bool
pieceIsRedBlockedCow b (x, y) = getPiece b (x, y) == Just (Red Cow) && length (validCowMoves b (x, y)) == 0

gameIsWon :: Board -> Player -> Bool
gameIsWon b BluePlayer = or [ pieceIsBlueBlockedCow b (x, y) | x <- [0, 1, 2, 3], y <- [0, 1, 2, 3] ]
gameIsWon b RedPlayer = or [ pieceIsRedBlockedCow b (x, y) | x <- [0, 1, 2, 3], y <- [0, 1, 2, 3] ]


{-| 
  Finale: Implement a computer player to play the game of Bean's Gambit.
  Given the history of the game as a (nonempty) list, the AI should return an 
  appropriate next move as a pair (from, to).

  [JUSTIFY]
-}
nextMove :: [Board] -> Player -> (Coord, Coord)
nextMove b p = case p of
  BluePlayer -> nextMoveBlue b h
  RedPlayer -> ((1, 1), (1, 2))
  where
    h = head b

nextMoveBlue :: [Board] -> Board -> (Coord, Coord)
nextMoveBlue b h 
  {- If blue has more beans, pick whichever bean is furthest back and try to move it forward -}
  | fst a                                                 = snd a
  {- Otherwise, try and move cow forwards (to make it less likely to be blocked) -}
  | fst b                                                 = snd b
  {- Otherwise, try and capture an opposing bean nearest to your end of the board by moving forwards or sideways, prefering to move forward -}
  | fst c                                                 = snd b
  where a = moveFurthestBackBeanForward h BluePlayer 
        b = moveFurthestBackCowForward h BluePlayer
        c = captureOpposingBean h BluePlayer
  
moveFurthestBackBeanForward :: Board -> Player -> (Bool, (Coord, Coord))
moveFurthestBackBeanForward b BluePlayer 
  | balance b < 0 && beanForwardMoves /= []          = (True, head beanForwardMoves)
  | otherwise                                                     = (False, ((0, 0), (0, 0)))
  where beanForwardMoves = [ ((x, y), (x, y + 1)) | x <- [0, 1, 2, 3], y <- [0, 1, 2, 3], (x, y + 1) `elem` validBeanMoves b BluePlayer (x, y)] 

moveFurthestBackCowForward :: Board -> Player -> (Bool, (Coord, Coord))
moveFurthestBackCowForward b BluePlayer 
  | cowForwardMoves /= []          = (True, head cowForwardMoves)
  | otherwise                                                     = (False, ((0, 0), (0, 0)))
  where cowForwardMoves = [ ((x, y), (x, y + 1)) | x <- [0, 1, 2, 3], y <- [0, 1, 2, 3], (x, y + 1) `elem` validCowMoves b (x, y)] 

captureOpposingBean :: Board -> Player -> (Bool, (Coord, Coord))
captureOpposingBean b BluePlayer
  | captureForwardsMoves /= 0     = (True, head captureForwardsMoves)
  | captureEastMoves /= 0         = (True, head captureEastMoves)
  | captureWestMoves /= 0         = (True, head captureWestMoves)
  | otherwise                     = (False, ((0, 0), (0, 0)))
  where captureForwardsMoves = [ ((x, y), (x, y + 1)) | x <- [0, 1, 2, 3], y <- [0, 1, 2, 3], (x, y + 1) `elem` validBeanMoves b BluePlayer (x, y) && getPiece (x, y+1) == Red Bean ]
        captureEastMoves = [((x, y), (x + 1, y)) | (x + 1, y) `elem` validBeanMoves b BluePlayer (x, y) && getPiece (x + 1, y) == Red Bean]
        captureWestMoves = [((x, y), (x - 1, y)) | (x - 1, y) `elem` validBeanMoves b BluePlayer (x, y) && getPiece (x - 1, y) == Red Bean]