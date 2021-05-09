
{-# LANGUAGE NegativeLiterals #-}
--
-- Spec.hs is for testing the functions of the program.
--
--
-- Used for property testing
{-# LANGUAGE TemplateHaskell #-}


import Test.DocTest (doctest)
import Board 
import Game 

main :: IO ()
main = do
    
    doctest ["-isrc", "src/Board.hs",  "src/Game.hs"]

