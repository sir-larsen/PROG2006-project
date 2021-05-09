module Game where

import Prelude hiding (getContents)
import Board

convert :: Bool -> String
convert True = ""
convert False = "Illegal Move!\n"