{-# LANGUAGE DuplicateRecordFields #-}

module Engine.NEAT ( module SSMonad
                   , module SSNumeric
                   , module Genetics.Genes
                   , module Genetics.Neurons
                   , module Genetics.Critters
                   , module Population.Population
                   , module Evolution.Goals
                   , pPrint
                   ) where

import SSMonad
import SSNumeric
import Engine.NEAT.Genetics.Genes
import Engine.NEAT.Genetics.Neurons
import Engine.NEAT.Genetics.Critters
import Engine.NEAT.Population.Population
import Engine.NEAT.Evolution.Goals
import Text.Pretty.Simple (pPrint)
-- import NEAT (SSNumeric(SSDouble))

default (Double)
-- >>> 2 + 2
