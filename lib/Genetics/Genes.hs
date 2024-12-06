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

data (SSNum a) => NType a = Pyramidal  { activation     :: a -> a
                                      , depolarization :: a } 
                         | Purkinje   { activation     :: a -> a
                                      , depolarization :: a
                                      , rate :: Float }
                         | Regular    { activation     :: a -> a }
                         | Inhibitory { activation     :: a -> a }

instance (SSNum a) => Show (NType a) where
    show :: SSNum a => NType a -> String
    show (Pyramidal _ dep) = "Pyramidal with depolarization: " ++ show dep
    show (Purkinje _ dep rate) = "Purkinje with depolarization: " ++ show dep ++ ", rate: " ++ show rate
    show (Regular _) = "Regular Neuron"
    show (Inhibitory _) = "Inhibitory Neuron"

-- neuron constructor functions
mkPyramidal'      :: SSNum a => (a -> a) -> a -> NType a
mkPyramidal' f d  = Pyramidal f d

mkPurkinje'       :: SSNum a => (a -> a) -> a -> Float -> NType a
mkPurkinje' f d r = Purkinje f d r

mkRegular'        :: SSNum a => (a -> a) -> NType a
mkRegular' f      = Regular f

mkInhiborty'      :: SSNum a => (a -> a) -> NType a
mkInhiborty' f    = Inhibitory f

-- neuron constructor functions with sigmoid and other activations
-- TODO: modify the activation functions to what they should be.
mkPyramidal    :: SSNum a => a -> NType a
mkPyramidal d  = mkPyramidal' (\x -> 1 / (1 + exp (-x))) d

mkPurkinje     :: SSNum a => a -> Float -> NType a
mkPurkinje d r = mkPurkinje' (\x -> 1 / (1 + exp (-x))) d r

mkRegular      :: SSNum a => NType a
mkRegular      = mkRegular' (\x -> 1 / (1 + exp (-x)))

mkInhiborty    :: SSNum a => NType a
mkInhiborty    = mkInhiborty' (\x -> 1 / (1 + exp (-x))) 

-- TODO: remove this if we dont need the Eq
instance SSNum a => Eq (NType a) where
    (==) :: SSNum a => NType a -> NType a -> Bool
    (Pyramidal _ dep1) == (Pyramidal _ dep2) = dep1 == dep2
    (Purkinje _ dep1 rate1) == (Purkinje _ dep2 rate2) = dep1 == dep2 && rate1 == rate2
    (Regular _) == (Regular _) = True
    (Inhibitory _) == (Inhibitory _) = True
    _ == _ = False  -- Different constructors are not equal
    
data Role = Input
          | Output
          | Hidden
          deriving (Show, Eq)

data SSNum a => Node a = Node { ntype    :: NType a
                             , role     :: Role
                             } deriving (Show, Eq)

data SSNum a => Connection a = Connection { innovation :: SN Int
                                         , node_in    :: Int
                                         , node_out   :: Int
                                         , weight     :: a
                                         , enabled    :: Bool
                                         }

instance (SSNum a) => Show (Connection a) where
  show (Connection _innov nin nout w en) = "conn innov: " ++ innovShow
    ++ " node_in: "  ++ show nin
    ++ " node_out: " ++ show nout
    ++ " weight: "   ++ show w
    ++ " enabled: "  ++ show en
    where
      innovShow = "<innov>" -- TODO: Get innovation show working
  

instance (SSNum a) => Eq (Connection a) where
  (==) :: Connection a -> Connection a -> Bool
  (Connection _innov1 nin1 nout1 w1 en1)
    == (Connection _innov2 nin2 nout2 w2 en2) = nin1 == nin2
    && nout1 == nout2
    && w1 == w2
    && en1 == en2
