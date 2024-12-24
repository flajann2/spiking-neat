{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Genetics.GeneTypes where

import SSNumeric

data NType = Pyramidal  { activation     :: SSNumeric -> SSNumeric
                        , depolarization :: SSNumeric } 
           | Purkinje   { activation     :: SSNumeric -> SSNumeric
                        , depolarization :: SSNumeric
                        , rate :: Float }
           | Regular    { activation     :: SSNumeric -> SSNumeric }
           | Inhibitory { activation     :: SSNumeric -> SSNumeric }

instance Show NType where
    show :: NType -> String
    show (Pyramidal _ dep) = "Pyramidal with depolarization: " ++ show dep
    show (Purkinje _ dep rate) = "Purkinje with depolarization: " ++ show dep ++ ", rate: " ++ show rate
    show (Regular _) = "Regular Neuron"
    show (Inhibitory _) = "Inhibitory Neuron"

instance Eq NType where
    (==) :: NType -> NType -> Bool
    (Pyramidal _ dep1) == (Pyramidal _ dep2) = dep1 == dep2
    (Purkinje _ dep1 rate1) == (Purkinje _ dep2 rate2) = dep1 == dep2 && rate1 == rate2
    (Regular _) == (Regular _) = True
    (Inhibitory _) == (Inhibitory _) = True
    _ == _ = False  -- Different constructors are not equal
