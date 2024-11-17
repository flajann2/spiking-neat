module SNMonad ( module SNMonad
               , module Control.Monad.Trans.State
               , module Data.Semigroup
               , liftIO
               , Config(..)
               ) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup
import Genetics.Neurons
import Evolution.Goals

data Config = Config { population_size :: Int
                     , neuron_types :: [Neuron]
                     , goal :: Goal
                     , sequence_number :: Int
                     , innovation_number :: Int
                     -- TODO: the following two should be
                     -- TODO: dealt with differently
                     , num_inputs :: Int
                     , num_outputs :: Int
                     } deriving Show

type SN = StateT Config IO

initialConfig :: Config
initialConfig = Config { population_size = 100
                       , neuron_types = [Neuron]
                       , goal = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       , num_inputs = 20
                       , num_outputs = 5
                       }

getConfig :: SN Config
getConfig = get

updateConfig :: Config -> SN ()
updateConfig newconf = put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up.
nextSequenceNumber :: SN Int
nextSequenceNumber = do
  config <- getConfig
  let next_seq = sequence_number config
  let uconf = config {sequence_number = next_seq + 1 }
  updateConfig uconf
  return next_seq

nextInnovationNumber :: SN Int
nextInnovationNumber = do
  config <- getConfig
  let next_innov = innovation_number config
  let uconf = config {innovation_number = next_innov + 1 }
  updateConfig uconf
  return next_innov
