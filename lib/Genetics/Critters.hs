{-# LANGUAGE DataKinds #-}

module Genetics.Critters where

import Genetics.Genes

data Critter = Critter { nodes          :: [Node]
                       , inputs         :: [Int] -- indices of the input nodes
                       , outputs        :: [Int] -- indices of the output nodes
                       , hidden         :: [Int] -- indices of the hidden nodes
                       , connections    :: [Connection]
                       , number_inputs  :: Int
                       , number_outputs :: Int
                       } deriving Show

node :: NType -> Role -> Node
node nt r = Node { ntype = nt
                 , role = r }

conn :: Int -> Int -> Connection
conn in' out' = Connection { innovation = 0
                           , node_in = in'
                           , node_out = out'
                           , weight = 0
                           , enabled = True                             
                           }

mkCritter :: [Node] -> [Connection] -> Int -> Int -> Critter
mkCritter n c num_in num_out = Critter { nodes = n
                                       , connections = c
                                       , number_inputs = num_in
                                       , number_outputs = num_out
                                       , inputs = findInputs
                                       , outputs = findOutputs
                                       , hidden = findHidden
                                       }
                               where
                                 findInputs = undefined
                                 findOutputs = undefined
                                 findHidden = undefined
