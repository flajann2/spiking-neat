{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021, OverloadedRecordDot #-}

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

mkCritter :: [Node] -> [Connection] -> SN Critter
mkCritter ns cs = do
  cfg <- getConfig
  crit' <- evalStateT ( do
                         let crit = Critter { nodes          = ns
                                            , connections    = cs
                                            , number_inputs  = cfg.num_inputs
                                            , number_outputs = cfg.num_outputs
                                            , inputs         = findInputs
                                            , outputs        = findOutputs
                                            , hidden         = findHidden
                                            }
                         return crit
                     ) cfg
  return crit'
    where
      findInputs = undefined
      findOutputs = undefined
      findHidden = undefined

