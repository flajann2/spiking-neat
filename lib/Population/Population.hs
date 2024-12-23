{-# LANGUAGE DatatypeContexts #-}

module Population.Population where

import Genetics.Critters
-- import SSMonad
import SSNumeric

data Population = Population { critters :: [Critter]
                             , epsilon :: [Maybe Float]
                             } deriving Show

