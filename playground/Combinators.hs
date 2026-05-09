{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Engine.AI

-- ================================================================
-- Example Layers
-- ================================================================
data LinearLayer = LinearLayer { weights :: [Double], bias :: Double }

instance Engine LinearLayer where
  type Input LinearLayer = [Double]
  type Output LinearLayer = Double
  infer (LinearLayer w b) xs = sum (zipWith (*) w xs) + b

instance Trainable LinearLayer where
  type Dataset LinearLayer = [([Double], Double)]
  train _ layer = layer  -- TODO: implement local update (NEAT mutation, HTM learning, etc.)

data ReLU = ReLU
instance Engine ReLU where
  type Input ReLU = Double
  type Output ReLU = Double
  infer ReLU x = max 0 x

data Sigmoid = Sigmoid
instance Engine Sigmoid where
  type Input Sigmoid = Double
  type Output Sigmoid = Double
  infer Sigmoid x = 1 / (1 + exp (-x))

data FixedLookup = FixedLookup
instance Engine FixedLookup where
  type Input FixedLookup = String
  type Output FixedLookup = [Double]
  infer FixedLookup "cat" = [0.1, 0.9, -0.2]
  infer FixedLookup "dog" = [0.8, -0.3, 0.5]
  infer FixedLookup _     = [0, 0, 0]

-- ================================================================
-- Composed Model Examples
-- ================================================================
composed :: Composed FixedLookup (Composed LinearLayer (Composed ReLU (Composed LinearLayer Sigmoid)))
composed =
  FixedLookup
  ~>> LinearLayer [0.5, -0.1, 0.3] 0.1
  ~>> ReLU
  ~>> LinearLayer [0.7, -0.4] 0.0
  ~>> Sigmoid

result :: Double
result = infer composed "cat"

batchResults :: [String] -> [Double]
batchResults = inferMany composed

-- With adapter
adapted :: Composed LinearLayer (Adapter Double Int)
adapted = LinearLayer [1.0, 2.0] 0.0 >~> round

