{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHC2021, OverloadedRecordDot #-}

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

-- To allow for a generalization of numeric types
-- for example, complex numbers!
type SSNum a = (Num a
               , Show a
               , Fractional a
               , Floating a
               , Eq a)

data Config = Config { population_size :: Int
                     , neuron_types :: [Neuron]
                     , goal :: Goal
                     , sequence_number :: Int
                     , innovation_number :: Int
                     -- TODO: the following two should be
                     -- TODO: dealt with differently
                     , num_inputs :: Int
                     , num_outputs :: Int
                     } deriving Show

type SN = StateT Config IO

initialConfig :: Config
initialConfig = Config { population_size = 100
                       , neuron_types = [Neuron]
                       , goal = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       , num_inputs = 20
                       , num_outputs = 5
                       }

getConfig :: SN Config
getConfig = get

updateConfig :: Config -> SN ()
updateConfig newconf = put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up.
nextSequenceNumber :: SN Int

--- -- TODO: Rework the following to dedup and make simpler.
--- nextNumber :: Integral a => (Config -> a) -> (a -> Config -> Config) -> SN a
--- nextNumber fieldAcc fieldUpd = do
---   config <- getConfig
---   let next = fieldAcc config
---   let uconf = fieldUpd (next + 1) config
---   updateConfig uconf
---   return next

--- nextSequenceNumber :: SN Int64
--- nextSequenceNumber = do
---   return $ nextNumber sequence_number $ updateNumber sequence_number

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
