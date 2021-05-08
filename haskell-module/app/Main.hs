module Main where

import Data.Typeable
import Io

main :: IO ()
main = do
    s <- getMove
    putStrLn ("type of s is: " ++ (show (typeOf s)))
    print s
