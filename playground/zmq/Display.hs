{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Exit
import System.IO
import System.Environment
import System.ZMQ4.Monadic ( connect
                           , receive
                           , runZMQ
                           , socket
                           , subscribe
                           , liftIO
                           , Sub(Sub) )
import qualified Data.ByteString.Char8 as CS

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ do
        hPutStrLn stderr "usage: display <address> [<address>, ...]"
        exitFailure
    runZMQ $ do
        sub <- socket Sub
        subscribe sub ""
        mapM_ (connect sub) args
        forever $ do
            receive sub >>= liftIO . CS.putStrLn
            liftIO $ hFlush stdout
