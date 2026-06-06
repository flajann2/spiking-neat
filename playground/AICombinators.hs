{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Examples where

import Engine.AI
import System.Random (randomRIO, mkStdGen, RandomGen)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

-- ================================================================
-- 1. NEAT-inspired: Simple Mutable Linear Layer (Genetic style)
-- ================================================================

data NeatLinear = NeatLinear
  { nlWeights :: [Double]
  , nlBias    :: Double
  , nlMutationRate :: Double   -- probability of mutation
  }

instance Engine NeatLinear where
  type Input NeatLinear = [Double]
  type Output NeatLinear = [Double]
  infer (NeatLinear w b _) xs = [sum (zipWith (*) w xs) + b]

instance Trainable NeatLinear where
  type Dataset NeatLinear = [([Double], [Double])]   -- (input, target)

  -- NEAT-style training: mutate and keep the best
  train dataset layer@(NeatLinear w b mr) =
    let
      candidates = [mutate layer | _ <- [1..20]]          -- generate 20 mutants
      best = minimumByError dataset candidates
    in best

    where
      mutate (NeatLinear ws bs rate) = NeatLinear
        { nlWeights = map (mutateWeight rate) ws
        , nlBias    = mutateWeight rate bs
        , nlMutationRate = rate
        }

      mutateWeight rate x = do
        r <- randomRIO (-rate, rate)
        return (x + r)

      errorOf layer (x, target) =
        let pred = head (infer layer x)
            targ = head target
        in (pred - targ) ** 2

      totalError dataset layer = sum (map (errorOf layer) dataset)

      minimumByError ds = minimumBy (comparing (totalError ds))

      minimumBy cmp = head . sortOn cmp

-- ================================================================
-- 2. HTM-inspired: Simple Sparse Temporal Memory Predictor
-- ================================================================

data HTMCell = HTMCell
  { activeBits   :: [Int]      -- currently active columns
  , permanence   :: Map.Map Int Double   -- synapse permanence
  , threshold    :: Double
  }

data SimpleHTM = SimpleHTM
  { htmCells :: [HTMCell]
  , htmSize  :: Int            -- number of columns
  }

instance Engine SimpleHTM where
  type Input SimpleHTM = [Double]          -- dense input vector
  type Output SimpleHTM = [Double]         -- predicted next dense vector

  infer htm input =
    let active = take (htmSize htm `div` 10) $   -- sparse: top 10%
                   sortOn (negate . activation) [0..htmSize htm - 1]
    in map (\i -> if i `elem` active then 1.0 else 0.0) [0..htmSize htm - 1]

    where
      activation i = sum [ if abs (x - fromIntegral i) < 1.0 then 1.0 else 0.0
                         | x <- input ]

instance Trainable SimpleHTM where
  type Dataset SimpleHTM = [([Double], [Double])]   -- (input, next state)

  train dataset htm = htm   -- TODO: implement Hebbian + temporal pooling
                        -- For now: just return unchanged (placeholder)

-- ================================================================
-- Usage Example
-- ================================================================

exampleNeat :: IO ()
exampleNeat = do
  let layer = NeatLinear [0.5, -0.2, 0.1] 0.3 0.1
      dataSet = [ ([1,2,3], [4.5])
                , ([2,3,4], [6.8])
                , ([0,1,1], [1.2]) ]

  let trained = train dataSet layer
  print $ infer trained [1,2,3]
