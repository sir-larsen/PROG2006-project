module Board where

import Data.List
import Prelude hiding (getContents)

-- Datatypes ---------------------------------------------------------------------------------------
type Column = Int
type Row    = Int

data Player = Black | White
        deriving (Eq, Show)

data Board  = Con [[Player]] (Row, Column)
        deriving (Eq, Show)


-- Functions exclusively for the board ----------------------------------------------------------------

-- creates new board
createBoard:: (Row, Column) -> Board
createBoard (r, c) = Con (replicate c []) (r, c)

-- tranposes the board
transposer :: Board -> Board
transposer y@(Con x (r,c)) = playerToBoard ( transpose ( boardToPlayer y)) r c

-- generates array of players from board
boardToPlayer :: Board -> [[Player]]
boardToPlayer (Con x (r, c)) = x

-- generates board from array of players
playerToBoard :: [[Player]] -> Row -> Column -> Board
playerToBoard x r c = (Con x (r,c))


-- convert board to string
--
boardToString:: Board -> String
boardToString b@(Con x (r,c)) = (joinRows b 1) ++ generateLines (c-1)

-- generates the trailing lines for the output
generateLines :: Int -> String
generateLines a | a == -1 = "+\n"
    | otherwise = "+-" ++ generateLines (a-1) ++ " " ++ (show a)


--creates output for each cell
createOutput :: [Player] -> Int -> String
createOutput [] p = " |"
createOutput x 1 | head x ==Black = "#|"
  | head x ==White = "o|"
  | otherwise = " |"
createOutput x p | p > (length x) = " |"
  | otherwise = (createOutput (tail x) (p-1))

--seperated the cells of one row
separateCells :: [[Player]] -> Int -> String
separateCells [x] p = createOutput x p
separateCells (x:t) p = createOutput x p ++ separateCells t p

-- joins the rows together, original table is created from columns !
joinRows :: Board -> Int -> String
joinRows (Con x (r, c)) p | p >= (c-1) = "|" ++ separateCells x p ++ "\n"
         | otherwise = joinRows (Con x (r, c)) (p+1) ++ "|"  ++ separateCells x p ++ "\n"

-- ---------------------------------------------------------------------------------------

-- Game-logic

checkIfWin:: Board -> Column -> Player -> Bool
checkIfWin b c p | winner ( makeMove b c p ) == 1 = True
        | winner ( makeMove b c p ) == 2 = True
        | otherwise = False


--checks for winning cases in a column
--
columnWin :: [Player] -> Player -> Bool
columnWin [] p = False
columnWin (x:y:z:q:t) p | x==y && x==z && x ==q && x == p = True
        | otherwise = columnWin (y:z:q:t) p
columnWin (x:t) p = False

--checks for winning cases in a row
--
rowWin :: Board -> Player -> Bool
rowWin (Con [] (r, c)) p = False
rowWin (Con (x:t) (r, c)) p = (columnWin x p) || (rowWin  (Con t (r, c)) p)


-- Generate empty spaces for the other side
--
generateEmptySpaces :: Player -> Player
generateEmptySpaces p | p == Black = White
      | p == White = Black


--Gets the player-marker of position ( column , row)
--
getPlayer :: Board -> Int -> Int ->  Player ->  Player
getPlayer  (Con x (r, c)) a b p | b < length le = le !! b
            | otherwise = p
            where le | a < length x = x!!a
                            | otherwise = []

