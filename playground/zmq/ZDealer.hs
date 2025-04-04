{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Exit
import System.Environment
import System.ZMQ4
import Control.Monad
import Data.String
import Data.ByteString.Char8 (pack, unpack)
import Data.Restricted ()

main :: IO ()
main = withContext $ \ctx -> do
  args <- getArgs
  when (length args /= 1) $ do
      hPutStrLn stderr "usage: prompt <uniqueID>"
      exitFailure
  let uniqID = head args
  withSocket ctx Dealer $ \dealer ->
    do
      setIdentity (restrict (pack uniqID)) dealer
      connect dealer "tcp://localhost:5555"
        
      -- Send initial message
      send dealer [] $ pack $ "Hello from " <> uniqID
        
      -- Receive loop
      let recvLooper i = do
            response <- receive dealer
            case unpack response of
              "ACK"   -> putStrLn "Server acknowledged"
              "START" -> do
                putStrLn $ "Starting data transmission " <> show i <> "..."
                send dealer [] $ pack $ "PAYLOAD(" <> uniqID <> "): " <> show i
              _       -> putStrLn $ "Received: " <> unpack response
            recvLooper $ i+1
      recvLooper 0
