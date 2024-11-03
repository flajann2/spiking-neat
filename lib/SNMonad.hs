module SNMonad where

import Control.Monad.State
import Genetics.Neurons
import Evolution.Goals

data Config = Config { population_size :: Int
                     , neuron_types :: [Neuron]
                     , goal :: Goal
                     } deriving Show

type SN = State Config 



