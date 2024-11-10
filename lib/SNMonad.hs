{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021, OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module SNMonad ( module SNMonad
               , module Control.Monad.State
               , Int64
               ) where

import Data.Int (Int64)
import Control.Monad.State
import Genetics.Neurons
import Evolution.Goals
import Control.Lens
import Data.Default (Default, def)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Data.IORef
import Control.Monad (when)
import GHC.IOArray (newIOArray, writeIOArray)
-- import SNMonad (Config(population_size))

data Config = Config { population_size   :: IORef Int64
                     , neuron_types      :: IORef [Neuron]
                     , goal              :: IORef Goal
                     , sequence_number   :: IORef Int64
                     , innovation_number :: IORef Int64
                     } deriving (Generic)

-- TODO: revert to something safer 
instance Default Config where
    def = Config 
          { population_size   = unsafePerformIO $ newIORef 100
          , neuron_types      = unsafePerformIO $ newIORef [Neuron]
          , goal              = unsafePerformIO $ newIORef Goal
          , sequence_number   = unsafePerformIO $ newIORef 0
          , innovation_number = unsafePerformIO $ newIORef 0
          }

-- type SN = State Config 

initialConfig :: IO (IORef Config)
initialConfig = do
  config <- newIORef def
  return config

getConfig :: IORef Config
getConfig = do
  config <- get
  return config 
-- getConfig = do
--   config <- get
--   cf <- readIORef config
--   return cf

  
updateConfig :: Config -> IO ()
updateConfig newconf = put newconf

--- -- Function to update the sequence_number field
--- updateNumber :: Integral a => (Config -> a) -> a -> Config -> Config
--- updateNumber fieldAcc newVal cfg = cfg { fieldAcc = newVal }

--- -- TODO: Rework the following to dedup and make simpler.
--- nextNumber :: Integral a => (Config -> a) -> (a -> Config -> Config) -> SN a
--- nextNumber fieldAcc fieldUpd = do
---   config <- getConfig
---   let next = fieldAcc config
---   let uconf = fieldUpd (next + 1) config
---   updateConfig uconf
---   return next

--- nextSequenceNumber :: SN Int64
--- nextSequenceNumber = do
---   return $ nextNumber sequence_number $ updateNumber sequence_number

nextSequenceNumber :: IO Int64
nextSequenceNumber = do
  config <- readIORef getConfig
  current <- readIORef (sequence_number config)
  let next = current + 1
  writeIORef (sequence_number config) next
  return next

nextInnovationNumber :: IO Int64
nextInnovationNumber = do
  config <- readIORef getConfig
  current <- readIORef (innovation_number config)
  let next = current + 1
  writeIORef (innovation_number config) next
  return next
