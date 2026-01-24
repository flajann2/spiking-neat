{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Stream.Zio
import Control.Monad
import GHC.Generics (Generic)
import Data.Serialize ( Serialize )

{-
Test of the Object Dealer that will run outside of
the SpikingNeat Engine.
-}

data SimulatedDatum a = SimDat [a]
                      deriving ( Show
                               , Generic
                               , Serialize)

main :: IO ()
main = do
  startClient
  recdPopIDs
  forever $ do
    getDatum
    sendDatum2SN
    recdPopIDs
    
  where
    startStream   = undefined -- start client and receive  PopIDs
    recdPopIDs    = undefined
    getDatum      = undefined -- generate some data here to be streamed
    sendDatum2SN  = undefined
    getResults    = undefined -- result vectors should be processed in a different module?
    computeErrors = undefined
    sendErrors    = undefined
    sendEndStream = undefined 
    evaluate = do -- later this should be a seperate module
      getResults     
      computeErrors
      sendErrors
    
    
