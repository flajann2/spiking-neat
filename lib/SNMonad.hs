{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module SNMonad ( module SNMonad
               , module Control.Monad.Trans.State
               , module Data.Semigroup
               , module Data.Complex
               , liftIO
               , Config(..)
               ) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup
import Data.Complex (Complex)
import System.Random ( StdGen, Random(randomR), newStdGen )
import Control.Applicative (liftA2)
import GHC.Float (float2Double)
import SSNumeric

import {-# SOURCE #-} Genetics.Genes
import {-# SOURCE #-} Evolution.Goals
-- import {-# SOURCE #-} Genetics.Neurons

default (Double)


data Config = Config { population_size   :: Int
                     , neuron_types      :: [NType]
                     , goal              :: Goal
                     , sequence_number   :: Int
                     , innovation_number :: Int
                     , num_inputs        :: Int
                     , num_outputs       :: Int
                     , rng               :: IO StdGen -- TODO: We have nextRandom instead
                     , max_weight        :: SSNumeric
                     } 

instance Show Config where
  show (Config popize nt goal snum inum ninp nout rng maxw) =
       " population_size: "   ++ show popize  
    ++ " neuron_types: "      ++ show ntShow -- TODO fix this 
    ++ " goal: "              ++ show goal
    ++ " sequence_number: "   ++ show snum
    ++ " innovation_number: " ++ show inum
    ++ " num_inputs: "        ++ show ninp
    ++ " num_outputs: "       ++ show nout
    ++ " rng: "               ++ show rngShow
    ++ " max_weight: "        ++ show maxw
    where
      rngShow = "<IO StdGen>"
      ntShow = "<[NType]>"

type SN a = StateT Config IO a

-- initialConfig :: Config a
initialConfig :: Config
initialConfig = Config { population_size   = 100
                       , neuron_types      = [ Regular (\x -> x)
                                             , Inhibitory (\x -> x)
                                             ]
                       , goal              = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       , num_inputs        = 20
                       , num_outputs       = 5
                       , rng               = newStdGen
                       , max_weight        = SSDouble 5.0
                       }

getConfig :: StateT Config IO Config
getConfig = get

updateConfig :: Config -> SN ()
updateConfig newconf = put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up. Or not bother?
nextSequenceNumber :: SN Int
nextSequenceNumber = do
  config <- getConfig
  let next' = config.sequence_number
  let uconf = config { sequence_number = next' + 1 }
  updateConfig uconf
  pure next'

nextInnovationNumber :: SN Int
nextInnovationNumber = do
  config <- getConfig
  let next_innov = config.innovation_number
  let uconf = config {innovation_number = next_innov + 1 }
  updateConfig uconf
  return next_innov

nsi :: SN Int
nsi = nextSequenceNumber

nxi :: SN Int
nxi = nextInnovationNumber

nextRandom :: forall a. (SSNum a, Random a) => (a, a) -> SN a
nextRandom (from, to) = do
  cfg <- getConfig
  rn <- liftIO $ do
    gen <- cfg.rng
    let (randNum, _) = randomR (from, to) gen
    return randNum
  return rn

