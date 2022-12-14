module Board where

import Data.List
import Prelude hiding (interfacingFunction)

-- Datatypes ---------------------------------------------------------------------------------------
type Column = Int
type Row    = Int

data Player = Black | White
        deriving (Eq, Show)

data Board  = Con [[Player]] (Row, Column)
        deriving (Eq, Show)


-- Functions exclusively for the board ----------------------------------------------------------------

-- | creates new board
-- >>> createBoard (6, 7)
-- Con [[],[],[],[],[],[],[]] (6,7)
--
-- >>> createBoard (6, 3)
-- Con [[],[],[]] (6,3)
--
createBoard:: (Row, Column) -> Board
createBoard (r, c) = Con (replicate c []) (r, c)

-- | tranposes the board
-- >>> transposer (Con [[],[],[],[],[],[],[]] (6,7))
-- Con [] (6,7)
--
-- >>> transposer (Con [[],[],[],[],[],[],[]] (6,3))
-- Con [] (6,3)
--
transposer :: Board -> Board
transposer y@(Con x (r,c)) = playerToBoard ( transpose ( boardToPlayer y)) r c

-- | generates array of players from board
-- >>> boardToPlayer (Con [[],[],[],[],[],[],[]] (6,3))
-- [[],[],[],[],[],[],[]]
--
-- >>> boardToPlayer (Con [[],[],[],[],[]] (4,3))
-- [[],[],[],[],[]]
--
boardToPlayer :: Board -> [[Player]]
boardToPlayer (Con x (r, c)) = x


-- | generates board from array of players
-- >>> playerToBoard ([[],[],[],[],[]]) 6 3 
-- Con [[],[],[],[],[]] (6,3)
--
-- >>> playerToBoard ([[],[],[],[]]) 6 7 
-- Con [[],[],[],[]] (6,7)
--
playerToBoard :: [[Player]] -> Row -> Column -> Board
playerToBoard x r c = (Con x (r,c))


-- | convert board to string
-- >>> boardToString (Con [[],[],[],[],[]] (4,3)) 
-- "| | | | | |\n| | | | | |\n+-+-+-+\n 0 1 2"
--
boardToString :: Board -> String
boardToString b@(Con x (r,c)) = (joinRows b 1) ++ generateLines (c-1)


-- | generates the trailing lines for the output
-- >>> generateLines 1 
-- "+-+-+\n 0 1"
--
-- >>> generateLines 0
-- "+-+\n 0"
--
generateLines :: Int -> String
generateLines a | a == -1 = "+\n"
    | otherwise = "+-" ++ generateLines (a-1) ++ " " ++ (show a)


-- | creates output for each cell
createOutput :: [Player] -> Int -> String
createOutput [] p = " |"
createOutput x 1 | head x ==Black = "#|"
  | head x ==White = "o|"
  | otherwise = " |"
createOutput x p | p > (length x) = " |"
  | otherwise = (createOutput (tail x) (p-1))

-- | seperated the cells of one row
-- >>> separateCells [[],[],[],[]] 1 
-- " | | | |"
--
-- >>> separateCells [[],[],[],[],[]] 2 
-- " | | | | |"
--
separateCells :: [[Player]] -> Int -> String
separateCells [x] p = createOutput x p
separateCells (x:t) p = createOutput x p ++ separateCells t p

-- | joins the rows together, original table is created from columns !
-- >>> joinRows (Con [[],[],[],[],[]] (4,3)) 2 
-- "| | | | | |\n"
--
joinRows :: Board -> Int -> String
joinRows (Con x (r, c)) p | p >= (c-1) = "|" ++ separateCells x p ++ "\n"
         | otherwise = joinRows (Con x (r, c)) (p+1) ++ "|"  ++ separateCells x p ++ "\n"

-- ---------------------------------------------------------------------------------------

-- Game-logic



-- | Checks if the move results in a winning case
-- >>> checkIfWin (Con [[],[],[],[],[]] (4,3)) 2 Black
-- False
--
checkIfWin:: Board -> Column -> Player -> Bool
checkIfWin b c p | retrieveWinner ( move b c p ) == 1 = True
        | retrieveWinner ( move b c p ) == 2 = True
        | otherwise = False



-- | Utilizes the transposer to check winning cases both diagonally and through rows and columns
-- >>> checkIfWin2 (Con [[],[],[],[],[]] (4,3)) Black
-- False
--
checkIfWin2 :: Board -> Player -> Bool
checkIfWin2 x p = (checkLeft2 x p) || (checkRight2 x p) || (rowWin x p) || (rowWin (transposer x) p)


-- | Retrieves the winner if there is one
-- >>> retrieveWinner (Con [[],[],[],[],[]] (4,3))
-- 0
--
retrieveWinner :: Board -> Int
retrieveWinner x | checkIfWin2 x Black = 1
   | checkIfWin2 x White = 2
   | otherwise = 0



-- | checks for winning cases in a column
-- >>> columnWin [Black,Black,Black,Black,Black] Black
-- True
--
columnWin :: [Player] -> Player -> Bool
columnWin [] p = False
columnWin (x:y:z:q:t) p | x==y && x==z && x ==q && x == p = True
        | otherwise = columnWin (y:z:q:t) p
columnWin (x:t) p = False

-- | checks for winning cases in a row
-- >>> rowWin (Con [[],[],[],[],[]] (4,3)) Black
-- False
--
rowWin :: Board -> Player -> Bool
rowWin (Con [] (r, c)) p = False
rowWin (Con (x:t) (r, c)) p = (columnWin x p) || (rowWin  (Con t (r, c)) p)


-- | Generate empty spaces for the other side
-- >>> generateEmptySpaces Black
-- White
--
generateEmptySpaces :: Player -> Player
generateEmptySpaces p | p == Black = White
      | p == White = Black


-- | Gets the player-marker of position ( column , row)
-- >>> getPlayer (Con [[],[],[],[],[]] (4,3)) 6 7 Black
-- Black
--
getPlayer :: Board -> Int -> Int ->  Player ->  Player
getPlayer  (Con x (r, c)) a b p | b < length le = le !! b
            | otherwise = p
            where le | a < length x = x!!a
                            | otherwise = []

-- | Finds winning cases for right diagonals
-- >>> checkRightWins (Con [[],[],[],[],[]] (4,3)) Black 6 7
-- False
--
checkRightWins :: Board -> Player -> Int -> Int -> Bool
checkRightWins x p a b = q1 ==  q2 && q1 == q3 && q1 == q4 && q1 == p
      where
   p1 = generateEmptySpaces p
   q1 = getPlayer x a b p1
   q2 = getPlayer x (a+1) (b+1) p1
   q3 = getPlayer x (a+2) (b+2) p1
   q4 = getPlayer x (a+3) (b+3) p1


-- | helper ^
-- >>> checkRight (Con [[],[],[],[],[]] (4,3)) Black 6 7
-- False
--
checkRight :: Board -> Player -> Int -> Int -> Bool
checkRight x@(Con y (r, c)) p a b | a>c-4 && b > r - 5 = False
              | a>c-4 = checkRight x p 0 (b+1)
              | otherwise = checkRightWins x p a b || checkRight x p (a+1) b

-- | helper ^
-- >>> checkRight2 (Con [[],[],[],[],[]] (4,3)) Black 
-- False
--
checkRight2 :: Board -> Player -> Bool
checkRight2  x p = checkRight  x p 0 0


-- | Finds winning cases for left diagonals
-- >>> checkLeftWins (Con [[],[],[],[],[]] (4,3)) Black 6 7
-- False
--
checkLeftWins :: Board -> Player -> Int -> Int -> Bool
checkLeftWins x p a b = q1 ==  q2 && q1 == q3 && q1 == q4 && q1 == p
      where
   p1 = generateEmptySpaces p
   q1 = getPlayer x a b p1
   q2 = getPlayer x (a-1) (b+1) p1
   q3 = getPlayer x (a-2) (b+2) p1
   q4 = getPlayer x (a-3) (b+3) p1

-- | helper ^
-- >>> checkLeft (Con [[],[],[],[],[]] (4,3)) Black 6 7
-- False
--
checkLeft :: Board -> Player -> Int -> Int -> Bool
checkLeft x@(Con y (r,c)) p a b
            | a >= c && b < (r-4) = checkLeft x p 3 (b+1)
            | a >= c && b >= (r-4) = False
            | otherwise = checkLeftWins x p a b || checkLeft x p (a+1) b

-- | helper ^
-- >>> checkLeft2 (Con [[],[],[],[],[]] (4,3)) Black 
-- False
--
checkLeft2 :: Board -> Player -> Bool
checkLeft2  x@(Con y (r, c))  p | c > 3 = checkLeft x p 3 0
           | otherwise = False

-- | ReplayerTurns True if no winner is declared after board is full
-- >>> checkIfDraw (Con [[],[],[],[],[]] (4,3)) 
-- False
--
checkIfDraw:: Board -> Bool
checkIfDraw (Con [] z) = True
checkIfDraw (Con (x:xs) (r, c))
      | length x == r && checkIfDraw (Con xs (r, c)) == True = True
      | otherwise = False


-- | ReplayerTurns True if the move is legal
-- >>> checkIfLegal (Con [[],[],[],[],[]] (4,3)) 5
-- False
--
checkIfLegal:: Board -> Column -> Bool
checkIfLegal (Con [] (r, c)) z = True
checkIfLegal (Con x (r, c)) z | z >= c = False
           | otherwise = (kk < r)
              where
                kk = length k
                k  = (x !! z)


-- | Adds player to the board of if the move is legal
-- >>> move (Con [[],[],[],[],[]] (4,3)) 2 Black
-- Con [[],[],[Black],[],[]] (4,3)
--
move :: Board -> Column -> Player -> Board
move y@(Con board (r, c)) column player = Con (a ++ b ++ c') (r, c)
          where a  = (split1 board column)
                b  = [((board !! column) ++ [player])]
                c' = (split2 board column)



-- | The first half of the board. Helper function for move
split1:: [[Player]] -> Int -> [[Player]]
split1 board c = take c board


-- | The second half of the board. Helper function for move
split2:: [[Player]] -> Int -> [[Player]]
split2 board c = drop (c + 1) board


-- | Function necessary for our prelude.
-- >>> interfacingFunction (Con [[],[],[],[],[]] (4,3)) 
-- [[],[],[],[],[]]
--
interfacingFunction :: Board -> [[Player]]
interfacingFunction (Con [] z) = []
interfacingFunction (Con (x:xs) z) | null x == True = []: interfacingFunction (Con xs z)
         | null x == False = [last x]: interfacingFunction (Con xs z)

-- | creates rows within createboard
-- >>> createRow (Con [[],[],[],[],[]] (4,3)) 
-- 4
--
createRow :: Board -> Row
createRow (Con _ (r, _)) = r

-- | creates columns within createBoard
-- >>> createColumn (Con [[],[],[],[],[]] (4,3)) 
-- 3
--
createColumn :: Board -> Column
createColumn (Con _ (_, c)) = c

-- | returns the player whos turn it is
-- >>> playerTurn (Con [[],[],[],[],[]] (4,3)) 
-- Black
--
playerTurn :: Board -> Player
playerTurn (Con x (r, c))  | rc > yc = White
         | otherwise = Black
           where
      rc = length (filter (==Black) u1)
      yc = length (filter (==White) u1)
      u1 = concat x