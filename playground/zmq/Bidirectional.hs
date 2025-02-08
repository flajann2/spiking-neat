{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import System.ZMQ4 hiding (socket, bind, connect, send, receive)
import System.ZMQ4.Monadic ( liftIO
                           , runZMQ
                           , socket
                           , bind
                           , send
                           , connect
                           , receive
                           , async)

main :: IO ()
main = runZMQ $ do
  sender   <- socket Push
  receiver <- socket Pull

  bind sender "tcp://*:5555"
  connect receiver "tcp://localhost:5555"

  let sendLoop s i = do
        send sender [] (B.pack $ s ++ " : " ++ show i)
        sendLoop s (i+1)

  let receiveLoop = forever $ do
        msg <- receive receiver
        liftIO $ print msg
        -- receiveLoop 

  _ <- async $ sendLoop "World" 0
  -- _ <- async $ receiveLoop
  receiveLoop
  
  return ()
