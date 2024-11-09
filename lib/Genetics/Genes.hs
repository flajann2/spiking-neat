{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021, OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}

module Genetics.Genes where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import SNMonad

data NodeType py pu r i = Pyramidal py
                        | Purkinje pu
                        | Regular r
                        | Inhibitory i
                        deriving (Show, Eq)

data NodeRole = Input
              | Output
              | Hidden
              deriving (Show, Eq)

data NodeGene = NodeGene { seq_number :: Int64
                         , node_type  :: NodeType
                         , node_role  :: NodeRole                 
                         } deriving (Show, Eq)

data ConnectionGene = ConnectionGene { seq_number :: Int64
                                     , innovation_number :: Int64
                                     , node_in  :: Int64
                                     , node_out :: Int64
                                     , weight   :: Float
                                     , enabled  :: Bool
                                     } deriving (Show, Eq)
