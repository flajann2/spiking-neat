{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B
import System.ZMQ4
import System.ZMQ4.Monadic ( liftIO
                           , runZMQ
                           , socket
                           , bind
                           , send
                           , connect
                           , receive )

main :: IO ()
main = runZMQ $ do
  sender <- socket ZMQ_PUSH
  receiver <- socket ZMQ_PULL
  bind sender "tcp://*:5555"
  connect receiver "tcp://localhost:5555"

  let sendLoop s = do
      send s [] (B.pack "Hello")
      sendLoop s

  let receiveLoop r = do
      msg <- receive r
      liftIO $ print msg
      receiveLoop r

  liftIO $ forkIO $ sendLoop sender
  liftIO $ forkIO $ receiveLoop receiver
  return ()

--- import System.ZMQ4
--- import System.ZMQ4.Monadic ( liftIO
---                            , runZMQ
---                            , socket
---                            , bind
---                            , send
---                            , connect
---                            , receive )
--- import Control.Concurrent (forkIO)
--- import qualified Data.ByteString.Char8 as B
--- 
--- main :: IO ()
--- main = runZMQ $ do
---     withContext $ \ _ctx -> do
---         sender <- socket ZMQ_PUSH
---         receiver <- socket ZMQ_PULL
---         bind sender "tcp://*:5555"
---         connect receiver "tcp://localhost:5555"
--- 
---         let sendLoop s = do
---             send s [] (B.pack "Hello")
---             sendLoop s
--- 
---         let receiveLoop r = do
---             msg <- receive r
---             liftIO $ print msg
---             receiveLoop r
--- 
---         liftIO $ forkIO $ sendLoop sender
---         liftIO $ forkIO $ receiveLoop receiver
---         return ()

--- import Control.Concurrent
--- import Control.Monad
--- import Data.ByteString (ByteString)
--- import System.ZMQ4.Monadic
--- 
--- import System.ZMQ4 (withContext, Push, Pull)
--- main :: IO ()
--- main = withContext $ \ ctx -> do
---     let sender = socket ctx Push
---     receiver <- socket ctx Pull
--- 
---     bind sender "tcp://*:5555"
---     connect receiver "tcp://localhost:5555"
--- 
---     forkIO $ sendLoop ctx sender
---     receiveLoop ctx receiver
--- 
--- --sendLoop :: Context -> Socket -> IO ()
--- sendLoop ctx socket = forever $ do
---     send socket (pack "Hello from sender!") 0
--- 
--- --receiveLoop :: Context -> Socket -> IO ()
--- receiveLoop ctx socket = forever $ do
---     msg <- recv socket 0
---     putStrLn $ "Received: " ++ unpack msg

--- import Control.Concurrent
--- import Control.Monad
--- import Data.ByteString (ByteString)
--- import qualified System.ZMQ4 as Z -- (Context, withContext)
--- import System.ZMQ4.Monadic
--- 
--- main :: IO ()
--- main = Z.withContext $ \ctx -> do
---     sender <- socket ctx PUSH
---     receiver <- socket ctx PULL
--- 
---     bind sender "tcp://*:5555"
---     connect receiver "tcp://localhost:5555"
--- 
---     forkIO $ sendLoop ctx sender
---     receiveLoop ctx receiver
--- 
--- sendLoop :: Z.Context -> Z.Socket -> IO ()
--- sendLoop ctx socket = forever $ do
---     send socket (pack "Hello from sender!") 0
--- 
--- receiveLoop :: Z.Context -> Z.Socket -> IO ()
--- receiveLoop ctx socket = forever $ do
---     msg <- recv socket 0
---     putStrLn $ "Received: " ++ unpack msg
