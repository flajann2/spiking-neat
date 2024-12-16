{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}

module Genetics.Genes ( NType(..)
                      , Role(..)
                      , Node(..)
                      , Connection(..)
                      , mkPyramidal
                      , mkPurkinje
                      , mkRegular
                      , mkInhiborty
                      ) where

-- import qualified Data.HashMap.Strict as HM
-- import Data.Hashable (Hashable)
import SNMonad
import SSNumeric
import Genetics.GeneTypes

exp' :: SSNumeric -> SSNumeric
exp' (SSFloat x)  = SSFloat (exp x)
exp' (SSDouble x) = SSDouble (exp x)

-- neuron constructor functions
mkPyramidal'      :: (SSNumeric -> SSNumeric) -> SSNumeric -> NType
mkPyramidal' f d  = Pyramidal f d

mkPurkinje'       :: (SSNumeric -> SSNumeric) -> SSNumeric -> Float -> NType
mkPurkinje' f d r = Purkinje f d r

mkRegular'        :: (SSNumeric -> SSNumeric) -> NType
mkRegular' f      = Regular f

mkInhiborty'      :: (SSNumeric -> SSNumeric) -> NType 
mkInhiborty' f    = Inhibitory f

-- neuron constructor functions with sigmoid and other activations
-- TODO: modify the activation functions to what they should be.
mkPyramidal    :: SSNumeric -> NType 
mkPyramidal d  = mkPyramidal' (\x -> 1 / (1 + exp' (-x))) d

mkPurkinje     :: SSNumeric -> Float -> NType
mkPurkinje d r = mkPurkinje' (\x -> 1 / (1 + exp' (-x))) d r

mkRegular      :: NType
mkRegular      = mkRegular' (\x -> 1 / (1 + exp' (-x)))

mkInhiborty    :: NType 
mkInhiborty    = mkInhiborty' (\x -> 1 / (1 + exp' (-x))) 

-- TODO: remove this if we dont need the Eq
instance Eq NType where
    (==) :: NType -> NType -> Bool
    (Pyramidal _ dep1) == (Pyramidal _ dep2) = dep1 == dep2
    (Purkinje _ dep1 rate1) == (Purkinje _ dep2 rate2) = dep1 == dep2 && rate1 == rate2
    (Regular _) == (Regular _) = True
    (Inhibitory _) == (Inhibitory _) = True
    _ == _ = False  -- Different constructors are not equal
    
data Role = Input
          | Output
          | Hidden
          deriving (Show, Eq)

data Node = Node { ntype    :: NType
                 , role     :: Role
                 } deriving (Show, Eq)

data Connection = Connection { innovation :: SN Int
                             , node_in    :: Int
                             , node_out   :: Int
                             , weight     :: SSNumeric
                             , enabled    :: Bool
                             }

instance Show Connection where
  show (Connection _innov nin nout w en) = "conn innov: " ++ innovShow
    ++ " node_in: "  ++ show nin
    ++ " node_out: " ++ show nout
    ++ " weight: "   ++ show w
    ++ " enabled: "  ++ show en
    where
      innovShow = "<innov>" -- TODO: Get innovation show working
  

instance Eq Connection where
  (==) :: Connection -> Connection -> Bool
  (Connection _innov1 nin1 nout1 w1 en1)
    == (Connection _innov2 nin2 nout2 w2 en2) = nin1 == nin2
    && nout1 == nout2
    && w1 == w2
    && en1 == en2
