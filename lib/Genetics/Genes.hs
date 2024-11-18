{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021, OverloadedRecordDot #-}
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
-- import SNMonad

data NType = Pyramidal  { activation     :: Float -> Float
                        , depolarization :: Float } 
           | Purkinje   { activation     :: Float -> Float
                        , depolarization :: Float
                        , rate :: Float }
           | Regular    { activation     :: Float -> Float }
           | Inhibitory { activation     :: Float -> Float }

instance Show NType where
    show (Pyramidal _ dep) = "Pyramidal with depolarization: " ++ show dep
    show (Purkinje _ dep rate) = "Purkinje with depolarization: " ++ show dep ++ ", rate: " ++ show rate
    show (Regular _) = "Regular Neuron"
    show (Inhibitory _) = "Inhibitory Neuron"

-- neuron constructor functions
mkPyramidal'      :: (Float -> Float) -> Float -> NType
mkPyramidal' f d  = Pyramidal f d

mkPurkinje'       :: (Float -> Float) -> Float -> Float -> NType
mkPurkinje' f d r = Purkinje f d r

mkRegular'        :: (Float -> Float) -> NType
mkRegular' f      = Regular f

mkInhiborty'      :: (Float -> Float) -> NType
mkInhiborty' f    = Inhibitory f

-- neuron constructor functions with sigmoid and other activations
-- TODO: modify the activation functions to what they should be.
mkPyramidal    :: Float -> NType
mkPyramidal d  = mkPyramidal' (\x -> 1 / (1 + exp (-x))) d

mkPurkinje     :: Float -> Float -> NType
mkPurkinje d r = mkPurkinje' (\x -> 1 / (1 + exp (-x))) d r

mkRegular      :: NType
mkRegular      = mkRegular' (\x -> 1 / (1 + exp (-x)))

mkInhiborty    :: NType
mkInhiborty    = mkInhiborty' (\x -> 1 / (1 + exp (-x))) 

-- TODO: remove this if we dont need the Eq
instance Eq NType where
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

data Connection = Connection { innovation :: Int
                             , node_in    :: Int
                             , node_out   :: Int
                             , weight     :: Float
                             , enabled    :: Bool
                             } deriving (Show, Eq)

