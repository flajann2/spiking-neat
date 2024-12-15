{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Genetics.Critters where

import SNMonad 
import Genetics.Genes

data Critter = Critter { nodes          :: [Node]
                       , inputs         :: [Int] -- indices of the input nodes
                       , outputs        :: [Int] -- indices of the output nodes
                       , hidden         :: [Int] -- indices of the hidden nodes
                       , connections    :: [Connection]
                       , number_inputs  :: Int
                       , number_outputs :: Int
                       } deriving Show

class Eval where
  ecritter :: [a] -> SN [a]
  epopulation :: [a] -> SN [[a]]

node :: SSNum a => NType -> Role -> Node
node nt r = Node { ntype = nt
                 , role = r }

conn ::  Int -> Int -> SN Int -> Connection
conn in' out' innov = Connection { innovation = innov
                                 , node_in = in'
                                 , node_out = out'
                                 , weight = 0
                                 , enabled = True                             
                                 }

mkCritter :: SSNum a => [Node] -> [Connection] -> SN (Critter)
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

genCritter :: SN Critter 
genCritter = undefined
