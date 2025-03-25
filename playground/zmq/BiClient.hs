{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import System.ZMQ4
import qualified Data.ByteString.Char8 as C8
import Data.List.NonEmpty (NonEmpty(..))

main :: IO ()
main = withContext $ \ctx -> do
    -- Create a PAIR socket for bidirectional communication
    sock <- socket ctx Pair
    
    -- Connect the socket to the server's endpoint
    connect sock "tcp://localhost:5555"

    forever $ do
      -- Send a message to the server
      --send sock [] $ C8.pack "Hello from client!"
      sendMulti sock $ C8.pack "Hello from client!" :| []
      putStrLn "sent message to server."
    
      -- Receive a response from the server
      --msg <- receive sock
      msg <- receiveMulti sock
      putStrLn $ "Received: " ++ show msg
