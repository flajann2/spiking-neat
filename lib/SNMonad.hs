module SNMonad ( module SNMonad
               , module Control.Monad.State
               , Int64
               ) where

import Data.Int (Int64)
import Control.Monad.State
import Genetics.Neurons
import Evolution.Goals

data Config = Config { population_size :: Int64
                     , neuron_types :: [Neuron]
                     , goal :: Goal
                     , sequence_number :: Int64
                     , innovation_number :: Int64
                     } deriving Show

type SN = State Config

initialConfig :: Config
initialConfig = Config { population_size = 100
                       , neuron_types = [Neuron]
                       , goal = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       }

getConfig :: SN Config
getConfig = get

updateConfig :: Config -> SN ()
updateConfig newconf = put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up.
nextSequenceNumber :: SN Int64
nextSequenceNumber = do
  config <- getConfig
  let next_seq = sequence_number config
  let uconf = config {sequence_number = next_seq + 1 }
  updateConfig uconf
  return next_seq

nextInnovationNumber :: SN Int64
nextInnovationNumber = do
  config <- getConfig
  let next_innov = innovation_number config
  let uconf = config {innovation_number = next_innov + 1 }
  updateConfig uconf
  return next_innov
