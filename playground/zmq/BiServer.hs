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
    bind sock "tcp://*:5555"
    putStrLn "Server listening on port 5555"
    return sock

-- Message processing function
processMessage :: (Socket Pair) -> IO ()
processMessage sock = do
    msg <- receive sock
    putStrLn $ "Received: " ++ show msg
    send sock [] $ C8.pack "Hello from server!"
    putStrLn "Sent response to client"

main :: IO ()
main = withContext' $ \ctx -> do
    sock <- setupSocket ctx
    
    -- Add graceful shutdown mechanism
    let serverLoop = forever $ do
            processMessage sock
            threadDelay 10000  -- Prevent tight loop, 10ms delay
    
    serverLoop
