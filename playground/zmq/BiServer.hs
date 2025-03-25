module Main where

import Control.Monad
import qualified System.ZMQ4 as Z
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = Z.withContext $ \ctx -> do
    -- Create a PAIR socket for bidirectional communication
    socket <- Z.socket ctx Z.Pair
    
    -- Bind the socket to a TCP endpoint
    Z.bind socket "tcp://*:5555"
    
    putStrLn "Server listening on port 5555"
    
    -- Loop to receive and send messages
    forever $ do
        -- Receive a message
        msg <- Z.receive socket
        putStrLn $ "Received: " ++ show msg
        
        -- Send a response back
        Z.send socket [] $ C8.pack "Hello from server!"
