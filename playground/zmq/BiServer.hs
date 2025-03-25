module Main where

import Control.Monad
import System.ZMQ4
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = withContext $ \ctx -> do
    -- Create a PAIR socket for bidirectional communication
    sock <- socket ctx Pair
    
    -- Bind the socket to a TCP endpoint
    bind sock "tcp://*:5555"
    
    putStrLn "Server listening on port 5555"
    
    -- Loop to receive and send messages
    forever $ do
        -- Receive a message
        msg <- receive sock
        putStrLn $ "Received: " ++ show msg
        
        -- Send a response back
        send sock [] $ C8.pack "Hello from server!"
        putStrLn "sent response to client"
