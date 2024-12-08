{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Genetics.Neurons
import Evolution.Goals
import Data.Complex
import System.Random

-- To allow for a generalization of numeric types
-- for example, complex numbers!
type SSNum a = ( Num a
               , Show a
               , Fractional a
               , Floating a
               , Eq a)

data Config = Config { population_size   :: Int
                     , neuron_types      :: [Neuron]
                     , goal              :: Goal
                     , sequence_number   :: Int
                     , innovation_number :: Int
                     , num_inputs        :: Int
                     , num_outputs       :: Int
                     , rng               :: IO StdGen
                     } 

instance Show Config where
  show (Config popize nt goal snum inum ninp nout rng) =
       " population_size: "   ++ show popize  
    ++ " neuron_types: "      ++ show nt
    ++ " goal: "              ++ show goal
    ++ " sequence_number: "   ++ show snum
    ++ " innovation_number: " ++ show inum
    ++ " num_inputs: "        ++ show ninp
    ++ " num_outputs: "       ++ show nout
    ++ " rng: "               ++ show rngShow
    where
      rngShow = "<IO StdGen>"

type SN = StateT Config IO

initialConfig :: Config
initialConfig = Config { population_size   = 100
                       , neuron_types      = [Neuron]
                       , goal              = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       , num_inputs        = 20
                       , num_outputs       = 5
                       , rng               = newStdGen
                       }

getConfig :: SN Config
getConfig = get

updateConfig :: Config -> SN ()
updateConfig newconf = put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up. Or not bother?
nextSequenceNumber :: SN Int
nextSequenceNumber = do
  config <- getConfig
  let next = config.sequence_number
  let uconf = config { sequence_number = next + 1 }
  updateConfig uconf
  return next

nextInnovationNumber :: SN Int
nextInnovationNumber = do
  config <- getConfig
  let next_innov = innovation_number config
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

