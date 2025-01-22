{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import System.ZMQ4.Monadic

import System.ZMQ4 (withContext, Push, Pull)
main :: IO ()
main = withContext $ \ctx -> do
    sender <- socket ctx Push
    receiver <- socket ctx Pull

    bind sender "tcp://*:5555"
    connect receiver "tcp://localhost:5555"

    forkIO $ sendLoop ctx sender
    receiveLoop ctx receiver

--sendLoop :: Context -> Socket -> IO ()
sendLoop ctx socket = forever $ do
    send socket (pack "Hello from sender!") 0

--receiveLoop :: Context -> Socket -> IO ()
receiveLoop ctx socket = forever $ do
    msg <- recv socket 0
    putStrLn $ "Received: " ++ unpack msg

--- import Control.Concurrent
--- import Control.Monad
--- import Data.ByteString (ByteString)
--- import qualified System.ZMQ4 as Z -- (Context, withContext)
--- import System.ZMQ4.Monadic
--- 
--- main :: IO ()
--- main = Z.withContext $ \ctx -> do
---     sender <- socket ctx PUSH
---     receiver <- socket ctx PULL
--- 
---     bind sender "tcp://*:5555"
---     connect receiver "tcp://localhost:5555"
--- 
---     forkIO $ sendLoop ctx sender
---     receiveLoop ctx receiver
--- 
--- sendLoop :: Z.Context -> Z.Socket -> IO ()
--- sendLoop ctx socket = forever $ do
---     send socket (pack "Hello from sender!") 0
--- 
--- receiveLoop :: Z.Context -> Z.Socket -> IO ()
--- receiveLoop ctx socket = forever $ do
---     msg <- recv socket 0
---     putStrLn $ "Received: " ++ unpack msg
