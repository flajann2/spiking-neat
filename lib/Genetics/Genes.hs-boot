
-- partial definitions to break cyclic dependencies
module Genetics.Genes where

import SSNumeric

data NType = Pyramidal  { activation     :: SSNumeric -> SSNumeric
                        , depolarization :: SSNumeric } 
           | Purkinje   { activation     :: SSNumeric -> SSNumeric
                        , depolarization :: SSNumeric
                        , rate :: Float }
           | Regular    { activation     :: SSNumeric -> SSNumeric }
           | Inhibitory { activation     :: SSNumeric -> SSNumeric }