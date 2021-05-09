module Main where

import Data.Typeable
import Io

main :: IO ()
main = do
    {-|s <- getMove
    putStrLn ("type of s is: " ++ (show (typeOf s)))
    print s
    putStrLn s
    --if s == "\""cock\n"\"" then putStrLn "HOI"
    --else
    --    putStrLn "NEY"
    let f = words s
    print f
    print (f !! 0)
    let x = (f !! 0)
    
    if x == "3" then putStrLn "HOI"
    else
        putStrLn "NEY"-}
    sendBoard "| | | | | | |\n| | | | | | |\n"
    publisher