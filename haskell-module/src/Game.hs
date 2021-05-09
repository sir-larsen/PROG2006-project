module Game where

import Prelude hiding (getContents)
import Board
import Data.Typeable
import Io

-- | Checks if a move is legal or not
checkLegalMove :: Bool -> String
checkLegalMove True = ""
checkLegalMove False = "Illegal Move!\n"


-- | Returns True if the move is valid
isValidMove :: Board -> Column -> IO Bool
isValidMove b c = do
                let x = checkIfLegal b c
                putStr (checkLegalMove x)
                return (x)


-- | Takes a string from the user and parses it into Int
getInput :: Player -> IO Int
getInput player = do
                     --putStr "\nPlease enter move for "
                     putStr (show player)
                     putStrLn ": "

                     let a = ("\nPlease enter move for " ++ (show player) ++ " between 0-6")
                     sendBoard(a)

                     x <-getMove

                     --x<-getLine
                     return (read x)

-- | Cases for the program to end. Either a win or a draw
draw        =  "Its a draw!"
whitewins    = "White is the winner!"
blackwins   =  "Black is the winner"



-- | Gameloop running continuously
gameLoop :: Board -> IO ()
gameLoop board 
           | retrieveWinner board == 1 = do
             sendBoard blackwins
             putStrLn blackwins
           | retrieveWinner board == 2 = do
             sendBoard whitewins
             putStrLn whitewins
           | checkIfDraw board == True = do
             sendBoard "draw it is"
             putStrLn draw
           | otherwise = do
             x <- getInput (playerTurn board)
             putStrLn ("type of s is: " ++ (show (typeOf x)))
             ok <- isValidMove board x
             let board1 | ok  = move board x (playerTurn board)
                        | otherwise = board
             --temp <- putStrLn ("\n" ++ (boardToString board1))
             let a = boardToString board1
             print(a)
             sendBoard a
             gameLoop board1

