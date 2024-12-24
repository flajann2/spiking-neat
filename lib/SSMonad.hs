{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MonoLocalBinds #-}

module SSMonad ( module SSMonad
               , module Control.Monad.Trans.State
               , module Data.Semigroup
               , module Data.Complex
               , liftIO
               , Config(..)
               ) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Semigroup
import Data.Complex (Complex)
import System.Random ( StdGen, Random(randomR), newStdGen )
import SSNumeric

import Genetics.GeneTypes
import Evolution.GoalTypes

default (Double)

data Config = Config { population_size   :: Int
                     , neuron_types      :: [NType]
                     , goal              :: Goal
                     , sequence_number   :: Int
                     , innovation_number :: Int
                     , num_inputs        :: Int
                     , num_outputs       :: Int
                     , rng               :: IO StdGen -- use nextRandom instead
                     , max_weight        :: SSNumeric
                     } 

instance Show Config where
  show (Config popize nt goal snum inum ninp nout rng maxw) =
       " population_size: "   ++ show popize  
    ++ " neuron_types: "      ++ show nt
    ++ " goal: "              ++ show goal
    ++ " sequence_number: "   ++ show snum
    ++ " innovation_number: " ++ show inum
    ++ " num_inputs: "        ++ show ninp
    ++ " num_outputs: "       ++ show nout
    ++ " rng: "               ++ show rngShow
    ++ " max_weight: "        ++ show maxw
    where
      rngShow = "<IO StdGen>"

newtype SS a = SS { runSS :: StateT Config IO a }
             deriving ( Functor
                      , Applicative
                      , Monad
                      , MonadIO )
  
-- initialConfig :: Config a
initialConfig :: Config
initialConfig = Config { population_size   = 100
                       , neuron_types      = [ Regular (\x -> x)
                                             , Inhibitory (\x -> x)
                                             ]
                       , goal              = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       , num_inputs        = 10
                       , num_outputs       = 2
                       , rng               = newStdGen
                       , max_weight        = SSDouble 2.0
                       }

getConfig :: SS Config
getConfig = SS get

updateConfig :: Config -> SS ()
updateConfig newconf = SS $ put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up. Or not bother?
nextSequenceNumber :: SS Int
nextSequenceNumber = do
  config <- getConfig
  let next' = config.sequence_number
  let uconf = config { sequence_number = next' + 1 }
  updateConfig uconf
  pure next'

nextInnovationNumber :: SS Int
nextInnovationNumber = do
  config <- getConfig
  let next_innov = config.innovation_number
  let uconf = config {innovation_number = next_innov + 1 }
  updateConfig uconf
  return next_innov

nsi :: SS Int
nsi = nextSequenceNumber

nxi :: SS Int
nxi = nextInnovationNumber

nextRandom :: forall a. (SSNum a, Random a) => (a, a) -> SS a
nextRandom (from, to) = do
  cfg <- getConfig
  rn <- liftIO $ do
    gen <- cfg.rng
    let (randNum, _) = randomR (from, to) gen
    return randNum
  return rn
