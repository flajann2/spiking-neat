{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.ZMQ4
import Control.Monad
import Data.ByteString.Char8 (pack, unpack)
import Data.Restricted ()

main :: IO ()
main = withContext $ \ctx -> do
  withSocket ctx Dealer $ \dealer -> do
    setIdentity (restrict (pack "client1")) dealer
    connect dealer "tcp://localhost:5555"
        
    -- Send initial message
    send dealer [] "Hello"
        
    -- Receive loop
    forever $ do
      response <- receive dealer
      case (unpack response) of
        "ACK"   -> putStrLn "Server acknowledged"
        "START" -> do
          putStrLn "Starting data transmission..."
          send dealer [] "DATA_PAYLOAD"
        _       -> putStrLn $ "Received: " ++ unpack response



--- {-# LANGUAGE OverloadedStrings #-}
--- 
--- module Main where
--- 
--- import System.ZMQ4
--- import qualified Data.ByteString.Char8 as C
--- 
--- main :: IO ()
--- main = withContext $ \context -> do
---     -- Create a DEALER socket to send and receive messages from the server
---     dealerSocket <- socket context Dealer
---     connect dealerSocket "tcp://localhost:5555"
---     
---     let clientLoop i = do          
---           -- Send a message to the server
---           send dealerSocket []  C.pack $ "Dealer: " <> show i
---     
---           -- Receive the server's response
---           response <- receive dealerSocket
---           putStrLn $ "Router response: " ++ show response
---           clientLoop $ i+1
--- 
---     clientLoop 0
