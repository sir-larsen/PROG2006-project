{-# LANGUAGE OverloadedStrings #-}

module Io where

import Control.Concurrent
import Control.Monad
import Data.String
import System.IO
import System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as CS

-- | Function for getting a move from the Rust client via ZMQ
getMove :: IO String
getMove = runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub "tcp://127.0.0.1:5555" --Localhost
    let mv = receive sub
    let mv2 = CS.unpack <$> mv
    mv2

-- | Function for sending string via ZMQ, not always used for board
sendBoard :: String -> IO ()
sendBoard str = do
    let addr = "tcp://*:5000"
        name = ""
    runZMQ $ do
        pub <- socket Pub
        bind pub addr
        let board = CS.pack (str)
        liftIO $ threadDelay 300000 -- Have to use a delay so that the enter lines wont get rendered before the board
        send pub [] (name <> board)

-- | Publisher function from prog2006 git repo, used for testing and debug
publisher :: IO ()
publisher = do
    let addr = "tcp://*:5000"
        name = ""
    runZMQ $ do
        pub <- socket Pub
        bind pub addr
        forever $ do
            line <- liftIO $ fromString <$> getLine
            send pub [] (name <> line)