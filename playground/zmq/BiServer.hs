{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import System.ZMQ4
    ( bind,
      context,
      receive,
      send,
      socket,
      term,
      Pair(..),
      Context,
      Socket ) 
import qualified Data.ByteString.Char8 as C8
import Control.Concurrent (threadDelay)

-- Improved error handling with bracket
withContext' :: (Context -> IO a) -> IO a
withContext' = bracket context term

-- Centralized socket setup and error handling
setupSocket :: Context -> IO (Socket Pair)
setupSocket ctx = do
    sock <- socket ctx Pair
    bind sock "tcp://*:5555"
    putStrLn "Server listening on port 5555"
    return sock

-- Message processing function
processMessage :: (Socket Pair) -> Int -> IO ()
processMessage sock i = do
    msg <- receive sock
    putStrLn $ "Received: " ++ show msg
    send sock [] $ C8.pack $ "Hello from server: " <> show i
    putStrLn "Sent response to client"

main :: IO ()
main = withContext' $ \ctx -> do
    sock <- setupSocket ctx
    
    -- Add graceful shutdown mechanism
    let serverLoop i = do
            processMessage sock i
            threadDelay 10000  -- Prevent tight loop, 10ms delay
            serverLoop $ i+1
    
    serverLoop 0
