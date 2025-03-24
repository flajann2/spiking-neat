{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.String
import qualified Data.ByteString.Char8 as CS
import System.IO
import System.Exit
import System.Environment
import System.ZMQ4.Monadic ( bind
                           , runZMQ
                           , send
                           , socket
                           , liftIO
                           , Pub(Pub)
                           , Dealer(..) )
import Data.Serialize

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ do
        hPutStrLn stderr "usage: prompt <address> <username>"
        exitFailure
    let addr  = head args
        name  = fromString (args !! 1) <> ": "
        nameB = CS.pack name
    putStrLn $ "addr: " <> addr <> " name: " <> name
    runZMQ $ do
        pub <- socket Pub
        bind pub addr
        forever $ do
            line <- liftIO $ fromString <$> promptLine
            send pub [] (nameB <> line)
              where
                promptLine :: IO String
                promptLine = do
                  putStr ">> "
                  hFlush stdout
                  ll <- getLine
                  return ll
