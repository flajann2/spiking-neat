{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Control.Exception (bracket)
import System.ZMQ4 
import qualified Data.ByteString.Char8 as C8
import Control.Concurrent (threadDelay)

-- Improved error handling with bracket
withContext' :: (Context -> IO a) -> IO a
withContext' = bracket context term

-- Centralized socket setup and error handling
setupSocket :: Context -> IO (Socket Pair)
setupSocket ctx = do
    sock <- socket ctx Pair
    connect sock "tcp://localhost:5555"
    putStrLn "Client connected to server"
    return sock

-- Message exchange function
exchangeMessage :: Socket Pair -> Int -> IO ()
exchangeMessage sock i = do
    -- Send a message to the server
    send sock [] $ C8.pack $ "Hello from client: " <> show i
    putStrLn "Sent message to server"
    
    -- Receive a response from the server
    msg <- receive sock
    putStrLn $ "Received: " ++ show msg

main :: IO ()
main = withContext' $ \ctx -> do
    sock <- setupSocket ctx
    
    -- Add graceful message exchange loop
    let clientLoop i = forever $ do
          exchangeMessage sock i
          threadDelay 100000  -- 1 second delay between messages
          clientLoop $ i+1
    
    clientLoop 0
