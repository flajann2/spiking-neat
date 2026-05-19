{-# LANGUAGE DuplicateRecordFields #-}

module Engine.NEAT ( module SSMonad
                   , module SSNumeric
                   , module Engine.NEAT.Genetics.Genes
                   , module Engine.NEAT.Genetics.Neurons
                   , module Engine.NEAT.Genetics.Critters
                   -- , module Engine.NEAT.Population.Population
                   , module Engine.NEAT.Evolution.Goals
                   , pPrint
                   ) where

import SSMonad
import SSNumeric
import Engine.NEAT.Genetics.Genes
import Engine.NEAT.Genetics.Neurons
import Engine.NEAT.Genetics.Critters
-- import Engine.NEAT.Population.Population
import Engine.NEAT.Evolution.Goals
import Text.Pretty.Simple (pPrint)
-- import NEAT (SSNumeric(SSDouble))

default (Double)
-- >>> 6 * 2
-- 12
