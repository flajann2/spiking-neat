{-# LANGUAGE DatatypeContexts #-}

module Population.Population where

import Genetics.Critters
import SNMonad ( SSNum )

data (SSNum a) => Population a = Population { critters :: [Critter a]
                                         , epsilon :: [Maybe Float]
                                         } deriving Show

