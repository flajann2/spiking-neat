{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Data.Stream.Zio.Router
-- Description : 
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable
module Data.Stream.Zio.Router where

import Control.Concurrent
--- import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Char8 (unpack, ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import System.ZMQ4
import qualified Data.Set as Set

import Data.Stream.Zio.Common

routerForever ::IO (UniqueID, BSPayload) -> ((UniqueID, BSPayload) -> IO ()) -> IO ()
routerForever zrcv zresp = withContext $ \ctx -> do
    withSocket ctx Router $ \router -> do
      clientsVar <- startSN router
      popIDs <- prepInitialPopIDs
      sendPopIDs
        
      -- Main receive loop
      forever $ do
        receivePayload
        evaluateResults
        sendResults     -- send results to be asynchronously evaluatedf
        receiveEpsilons -- error vectors from the evaluated results
        evolvePopulations
        sendPopIDs
        -- 
        msg <- receiveMulti router
        case msg of
          [identity', content] -> do
            atomically $ modifyTVar clientsVar (Set.insert identity')
            putStrLn $ "Received: " ++ unpack content
            sendMulti router (identity' :| ["ACK"])
          _ -> putStrLn "Invalid message format"
      where
        startSN :: Socket Router -> IO (TVar (Set.Set ByteString))
        startSN router = do 
          bind router "tcp://*:5555"
          clientsVar <- newTVarIO Set.empty
          -- Broadcast thread
          _ <- forkIO $ broadcastLoop router clientsVar
          return clientsVar
          
        prepInitialPopIDs = undefined
        receivePayload    = undefined
        evaluateResults   = undefined
        sendResults       = undefined
        receiveEpsilons   = undefined
        evolvePopulations = undefined
        sendPopIDs        = undefined
          
broadcastLoop :: Socket Router -> TVar (Set.Set ByteString) -> IO ()
broadcastLoop router clientsVar = forever $ do
    threadDelay 100000  -- 0.1 seconds
    clients <- atomically $ readTVar clientsVar
    mapM_ (\clientId -> sendMulti router (clientId :| ["START"])) (Set.toList clients)
    -- putStrLn "Broadcasted START to all clients"
