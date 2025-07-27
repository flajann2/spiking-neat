{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Data.Stream.Zio.Common
-- Description : Common types and instances with Dealer and Router
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable
module Data.Stream.Zio.Common where

--- import System.IO
--- import System.Exit
--- import System.Environment
--- import System.ZMQ4
--- import Control.Monad
--- import Data.String
import Data.ByteString.Char8 (ByteString)
import Data.Restricted ()

newtype UniqueID  = UniqueID String deriving Show
data    BSPayload = BSPayload { bsSeq :: Int
                              , bs    :: ByteString} deriving Show
-- | protocol ACK
pACK   = "ACK"
-- | protocol START
pSTART = "START"
