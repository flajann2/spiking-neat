{-# LANGUAGE DatatypeContexts #-}

module Population.Population where

import Genetics.Critters
import SNMonad ( SSNumeric )

data Population = Population { critters :: [Critter]
                             , epsilon :: [Maybe Float]
                             } deriving Show

