{-# LANGUAGE DuplicateRecordFields #-}

module NEAT ( module SNMonad
            , module SSNumeric
            , module Genetics.Genes
            , module Genetics.Neurons
            , module Genetics.Critters
            , module Population.Population
            , module Evolution.Goals
            , pPrint
            ) where

import SNMonad
import SSNumeric
import Genetics.Genes
import Genetics.Neurons
import Genetics.Critters
import Population.Population
import Evolution.Goals
import Text.Pretty.Simple (pPrint)
-- import NEAT (SSNumeric(SSDouble))

default (Double)
