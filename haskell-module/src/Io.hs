{-# LANGUAGE OverloadedStrings #-}

module Io where

import Control.Concurrent
import Control.Monad
import Data.String
import System.IO
import System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as CS

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

getMove :: IO String
getMove = runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub "tcp://127.0.0.1:5555"
    let mv = receive sub
    let mv2 = CS.unpack <$> mv
    mv2

sendBoard :: String -> IO ()
sendBoard str = do
    let addr = "tcp://*:5000"
        name = ""
    runZMQ $ do
        pub <- socket Pub
        bind pub addr
        --let board = CS.pack ("MS AS A STRING")
        --let board = CS.pack (str ++ "\n\nPlease enter move:\n")
        let board = CS.pack (str)
        liftIO $ threadDelay 1000000
        send pub [] (name <> board)

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