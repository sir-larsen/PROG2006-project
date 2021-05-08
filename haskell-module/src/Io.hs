{-# LANGUAGE OverloadedStrings #-}

module Io where

import Control.Monad
import Data.String
import System.IO
import System.ZMQ4.Monadic
import Data.ByteString.Char8 as CS

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"

{-|subscriber :: IO ()
subscriber = do
    runZMQ $ do
        sub <- socket Sub
        subscribe sub ""
        connect sub "tcp://127.0.0.1:5555"
        forever $ do
            receive sub >>= liftIO . CS.putStrLn
            liftIO $ hFlush stdout
-}

subscriber :: IO ()
subscriber = do
    runZMQ $ do
        sub <- socket Sub
        subscribe sub ""
        connect sub "tcp://127.0.0.1:5555"
        forever $ do
            receive sub >>= liftIO . CS.putStrLn
            liftIO $ hFlush stdout