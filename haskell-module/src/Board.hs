module Board where

{-|import Data.List
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
--BoardToString :: Board -> String
--BoardToString b@(Con x (r,c)) = (joinRows b 1) ++ generateLines (c-1)

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
joinRows (Con x (r, c)) p | p >= (c-1) = "|" ++ seperateCells x p ++ "\n"
         | otherwise = joinRows (Con x (r, c)) (p+1) ++ "|"  ++ seperateCells x p ++ "\n"
-}
