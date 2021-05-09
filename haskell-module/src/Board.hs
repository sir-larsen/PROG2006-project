module Board where

import Data.List
import Prelude hiding (getContents)

type Column = Int
type Row    = Int

data Player = Black | White
        deriving (Eq, Show)