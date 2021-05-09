module Gamemain where

import Game
import Board  


gamemainf :: IO ()
gamemainf = gameLoop (createBoard (6, 7))
