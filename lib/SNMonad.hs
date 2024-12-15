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
import Genetics.Neurons
import Evolution.Goals
import Data.Complex (Complex)
import System.Random
import Control.Applicative (liftA2)
import GHC.Float (float2Double)

default (Double)

-- To allow for a generalization of numeric types
-- for example, complex numbers!
--- TODO Delete this block
type SSNum a = ( Num a
               , Show a
               , Fractional a
               , Floating a
               , Eq a)

data SSNumeric = SSFloat Float
               | SSDouble Double
               -- | SSComplex Complex a
               deriving Show

instance Eq SSNumeric where
  (SSFloat x) == (SSFloat y) = x == y
  (SSDouble x) == (SSDouble y) = x == y
  
instance Num SSNumeric where
    -- Negation
    negate (SSFloat x)  = SSFloat (negate x)
    negate (SSDouble x) = SSDouble (negate x)

    -- Addition
    (SSFloat x) + (SSFloat y)   = SSFloat (x + y)
    (SSDouble x) + (SSDouble y) = SSDouble (x + y)

    -- Subtraction
    (SSFloat x) - (SSFloat y)   = SSFloat (x - y)
    (SSDouble x) - (SSDouble y) = SSDouble (x - y)

    -- Multiplication
    (SSFloat x) * (SSFloat y)   = SSFloat (x * y)
    (SSDouble x) * (SSDouble y) = SSDouble (x * y)

    -- Absolute value
    abs (SSFloat x)  = SSFloat (abs x)
    abs (SSDouble x) = SSDouble (abs x)

    -- Signum function
    signum (SSFloat x)  = SSFloat (signum x)
    signum (SSDouble x) = SSDouble (signum x)

    -- Conversion from Integer
    fromInteger n = SSFloat  $ fromInteger n
    fromInteger n = SSDouble $ fromInteger n

instance Fractional SSNumeric where
    -- Division
    SSDouble x / SSDouble y = SSDouble (x / y)
    SSFloat x / SSFloat y   = SSFloat (x / y)
    
    -- Mixed division
    SSDouble x / SSFloat y  = SSDouble (x / float2Double y)
    SSFloat x / SSDouble y  = SSDouble (float2Double x / y)

    -- Reciprocal
    recip (SSFloat x)  = SSFloat (recip x)
    recip (SSDouble x) = SSDouble (recip x)

    -- Convert Rational to SSNumeric
    fromRational r = SSFloat $ fromRational r -- or convert to SSDouble if desired


data Config = Config { population_size   :: Int
                     , neuron_types      :: [Neuron]
                     , goal              :: Goal
                     , sequence_number   :: Int
                     , innovation_number :: Int
                     , num_inputs        :: Int
                     , num_outputs       :: Int
                     , rng               :: IO StdGen
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

type SN a = StateT Config IO a

-- initialConfig :: Config a
initialConfig :: Config
initialConfig = Config { population_size   = 100
                       , neuron_types      = [Neuron]
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

