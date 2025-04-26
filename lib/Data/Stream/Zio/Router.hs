{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

-- | Module    : Data.Stream.Zio.Router
-- Description : 
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable
module Data.Stream.Zio.Router where

import System.ZMQ4
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Char8 (unpack, ByteString)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))

import Data.Stream.Zio.Common

routerForever ::(IO (UniqueID, BSPayload)) -> ((UniqueID, BSPayload) -> IO ()) -> IO ()
routerForever zrcv zresp = withContext $ \ctx -> do
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
    threadDelay 100000  -- 0.1 seconds
    clients <- atomically $ readTVar clientsVar
    mapM_ (\clientId -> sendMulti router (clientId :| ["START"])) (Set.toList clients)
    -- putStrLn "Broadcasted START to all clients"
