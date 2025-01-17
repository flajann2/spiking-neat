module Population.Population where

import Genetics.Critters
import SSMonad ( SS )
-- import SSNumeric

data Population = Population { critters :: [Critter]
                             , epsilon :: [Maybe Float]
                             } deriving Show

genPopulation :: SS Population
genPopulation = undefined
