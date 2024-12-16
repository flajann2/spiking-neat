{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}

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
