{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields  #-}

-- | Module    : Data.Stream.Zio.Dealer
-- Description : Raw Payload Dealer
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable
module Data.Stream.Zio.Dealer where

import System.IO
import System.Exit
import System.Environment
import System.ZMQ4
import Control.Monad
import Data.String
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Data.Restricted ()

import Data.Stream.Zio.Common

-- | Loops forever, calling the send and receive functions you pass in.
--   You must supply a unique identifier. 
dealerForever :: UniqueID -> (BSPayload -> IO ()) -> (IO BSPayload) -> IO ()
dealerForever (UniqueID uniqID) zsnd zrcv = withContext $ \ctx -> do
  withSocket ctx Dealer $ \dealer ->
    do
      setIdentity (restrict (pack uniqID)) dealer
      connect dealer "tcp://localhost:5555"
        
      -- Send initial message
      -- send dealer [] $ pack $ "Hello from " <> uniqID
        
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
