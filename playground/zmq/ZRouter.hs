{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.ZMQ4
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Char8 (unpack, ByteString)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))

main :: IO ()
main = withContext $ \ctx -> do
    withSocket ctx Router $ \router -> do
      bind router "tcp://*:5555"
      clientsVar <- newTVarIO Set.empty
        
      -- Broadcast thread
      _ <- forkIO $ broadcastLoop router clientsVar
        
      -- Main receive loop
      forever $ do
        msg <- receiveMulti router
        case msg of
          [identity', content] -> do
            atomically $ modifyTVar clientsVar (Set.insert identity')
            putStrLn $ "Received: " ++ unpack content
            sendMulti router (identity' :| ["ACK"])
          _ -> putStrLn "Invalid message format"

broadcastLoop :: Socket Router -> TVar (Set.Set ByteString) -> IO ()
broadcastLoop router clientsVar = forever $ do
    threadDelay 5000000  -- 5 seconds
    clients <- atomically $ readTVar clientsVar
    mapM_ (\clientId -> sendMulti router (clientId :| ["START"])) (Set.toList clients)
    putStrLn "Broadcasted START to all clients"




--- {-# LANGUAGE OverloadedStrings #-}
--- 
--- module Main where
--- 
--- import System.ZMQ4
--- import qualified Data.ByteString.Char8 as C
--- 
--- main :: IO ()
--- main = withContext $ \context -> do
---     -- Create a ROUTER socket to accept multiple client connections
---     routerSocket <- socket context Router
---     bind routerSocket "tcp://*:5555"
---     
---     -- Accept and handle multiple client messages
---     let serverLoop i = do
---           -- Receive a message from a client
---           [clientAddress, message] <- receiveMulti routerSocket
---           putStrLn $ "Dealer response: <" ++ show clientAddress ++ ">: " ++ show message
---           -- Send a response back to the client
---           sendMulti routerSocket [clientAddress, (C.pack ("Router" <> show i))]
---           serverLoop $ i+1
--- 
---     serverLoop 0

