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


-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

-- creates new board
newBoard:: (Row, Column) -> Board
newBoard (r, c) = Con (replicate c []) (r, c)

-- tranposes the board
transboard :: Board -> Board
transboard y@(Con x (r,c)) = p2b ( transpose ( b2p y)) r c

-- generates array of players from board
b2p :: Board -> [[Player]]
b2p (Con x (r, c)) = x

-- generates board from array of players
p2b :: [[Player]] -> Row -> Column -> Board
p2b x r c = (Con x (r,c))


-- convert board to string
--
showBoard:: Board -> String
showBoard b@(Con x (r,c)) = (whr b 1) ++ trailer (c-1)

-- generates the trailing lines for the output
trailer :: Int -> String
trailer a | a == -1 = "+\n"
    | otherwise = "+-" ++ trailer (a-1) ++ " " ++ (show a)


--creates output for each cell
wh1 :: [Player] -> Int -> String
wh1 [] p = " |"
wh1 x 1 | head x ==Black = "#|"
  | head x ==White = "o|"
  | otherwise = " |"
wh1 x p | p > (length x) = " |"
  | otherwise = (wh1 (tail x) (p-1))

--seperated the cells of one row
wh :: [[Player]] -> Int -> String
wh [x] p = wh1 x p
wh (x:t) p = wh1 x p ++ wh t p

-- joins the rows together, original table is created from columns !
whr :: Board -> Int -> String
whr (Con x (r, c)) p | p >= (c-1) = "|" ++ wh x p ++ "\n"
         | otherwise = whr (Con x (r, c)) (p+1) ++ "|"  ++ wh x p ++ "\n"