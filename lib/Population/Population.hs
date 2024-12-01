module Population.Population where

import Genetics.Critters

data Population = Population { critters :: [Critter]
                             , epsilon :: [Maybe Float]
                             } deriving Show

