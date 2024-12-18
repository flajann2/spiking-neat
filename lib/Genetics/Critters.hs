{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}

module Genetics.Critters where

import SNMonad 
import Genetics.Genes
import Data.Kind ( Type )
import Data.Complex

data SSNum a => Critter a = Critter { nodes          :: [Node a]
                                   , inputs         :: [Int] -- indices of the input nodes
                                   , outputs        :: [Int] -- indices of the output nodes
                                   , hidden         :: [Int] -- indices of the hidden nodes
                                   , connections    :: [Connection a]
                                   , number_inputs  :: Int
                                   , number_outputs :: Int
                                   } deriving Show

class Num a => Eval a where
  ecritter :: [a] -> SN [a]
  epopulation :: [a] -> SN [[a]]

node :: SSNum a => NType a -> Role -> Node a
node nt r = Node { ntype = nt
                 , role = r }

conn ::  SSNum a => Int -> Int -> SN Int -> Connection a
conn in' out' innov = Connection { innovation = innov
                                 , node_in = in'
                                 , node_out = out'
                                 , weight = 0
                                 , enabled = True                             
                                 }

mkCritter :: SSNum a => [Node a] -> [Connection a] -> SN (Critter a)
mkCritter ns cs = do
  cfg <- getConfig
  let crit = Critter { nodes          = ns
                     , connections    = cs
                     , number_inputs  = cfg.num_inputs
                     , number_outputs = cfg.num_outputs
                     , inputs         = findInputs
                     , outputs        = findOutputs
                     , hidden         = findHidden
                     }
  return crit
    where
      findInputs :: [Int]
      findInputs = [i | (n, i) <- zip ns [0..], n.role == Input] 
      
      findOutputs :: [Int]
      findOutputs = [i | (n, i) <- zip ns [0..], n.role == Output]

      findHidden :: [Int]
      findHidden = [i | (n, i) <- zip ns [0..], n.role == Hidden]
