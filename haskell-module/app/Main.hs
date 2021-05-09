module Main where

import Data.Typeable
import Io
import Game
import Board

main :: IO ()
main = gameLoop (createBoard (6, 7))