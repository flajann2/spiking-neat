{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

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
-- import SSMonad
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
    
data Role = Input
          | Output
          | Hidden
          deriving (Show, Eq)

data Node = Node { ntype    :: NType
                 , role     :: Role
                 } deriving (Show, Eq)

data Connection = Connection { innovation :: Int
                             , node_in    :: Int
                             , node_out   :: Int
                             , weight     :: SSNumeric
                             , enabled    :: Bool
                             } deriving Show

instance Eq Connection where
  (==) :: Connection -> Connection -> Bool
  (Connection _innov1 nin1 nout1 w1 en1)
    == (Connection _innov2 nin2 nout2 w2 en2) = nin1 == nin2
    && nout1 == nout2
    && w1 == w2
    && en1 == en2
