module Main where

import Control.Monad
import qualified System.ZMQ4 as Z
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = Z.withContext $ \ctx -> do
    -- Create a PAIR socket for bidirectional communication
    socket <- Z.socket ctx Z.Pair
    
    -- Connect the socket to the server's endpoint
    Z.connect socket "tcp://localhost:5555"
    
    -- Send a message to the server
    Z.send socket [] $ C8.pack "Hello from client!"
    
    -- Receive a response from the server
    msg <- Z.receive socket
    putStrLn $ "Received: " ++ show msg
    
    -- You can continue sending and receiving in a loop
    -- for ongoing communication.
