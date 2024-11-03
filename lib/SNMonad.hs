module SNMonad where

import Control.Monad.State

data Config = Config { population_size :: Int
                     , neuron_types :: [Neuron]
                     , goal :: Goal
                     } deriving Show

type SN = State Config 
