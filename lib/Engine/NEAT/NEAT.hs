{-# LANGUAGE DuplicateRecordFields #-}

module NEAT ( module SSMonad
            , module SSNumeric
            , module Engine.NEAT.Genetics.Genes
            , module Engine.NEAT.Genetics.Neurons
            , module Engine.NEAT.Genetics.Critters
            , module Engine.NEAT.Population
            , module Engine.NEAT.Evolution.Goals
            , pPrint
            ) where

import SSMonad
import SSNumeric
import Genetics.Genes
import Genetics.Neurons
import Genetics.Critters
import Population.Population
import Evolution.Goals
import Text.Pretty.Simple (pPrint)
-- import NEAT (SSNumeric(SSDouble))

default (Double)
-- >>> 2 + 2
