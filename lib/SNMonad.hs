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
                       , sequence_number = 0
                       , innovation_number = 0
                       }

getConfig :: SN Config
getConfig = get

updateConfig :: Config -> SN ()
updateConfig newconf = put newconf

-- TODO: Rework the following to dedup and make simpler.
nextNumber :: Integral a => (Config -> a) -> (a -> Config -> Config) -> SN a
nextNumber fieldAcc fieldUpd = do
  config <- getConfig
  let next = fieldAcc config
  let uconf = fieldUpd (next + 1) config
  updateConfig uconf
  return next

nextSequenceNumber :: SN Int64
nextSequenceNumber = do
  return $ nextNumber sequence_number sequence_number

nextInnovationNumber :: SN Int64
nextInnovationNumber = do
  config <- getConfig
  let next = innovation_number config
  let uconf = config {innovation_number = next + 1 }
  updateConfig uconf
  return next
