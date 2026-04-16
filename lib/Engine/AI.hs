{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.AI where

import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))

-- ================================================================
-- Core Inference Engine
-- ================================================================
class Engine engine where
  type Input engine :: Type
  type Output engine :: Type
  infer :: engine -> Input engine -> Output engine

-- Batch inference (works on any Functor)
inferMany :: (Functor f, Engine e) => e -> f (Input e) -> f (Output e)
inferMany engine = fmap (infer engine)

-- ================================================================
-- Trainable Engines (Local / Modular Learning)
-- ================================================================
class Engine engine => Trainable engine where
  type Dataset engine :: Type
  -- Returns an updated engine after local training
  train :: Dataset engine -> engine -> engine

-- ================================================================
-- Composition
-- ================================================================
data Composed e1 e2 = Composed e1 e2

instance (Engine e1, Engine e2, Output e1 ~ Input e2) => Engine (Composed e1 e2) where
  type Input (Composed e1 e2)  = Input e1
  type Output (Composed e1 e2) = Output e2
  infer (Composed e1 e2) x = infer e2 (infer e1 x)

-- NEAT / HTM style Trainable instance for composition
-- Each submodule learns locally; downstream module acts as critic/teacher
instance (Trainable e1, Trainable e2,
          Output e1 ~ Input e2,
          Dataset e1 ~ [(Input e1, Output e1)],
          Dataset e2 ~ [(Input e2, Output e2)])
      => Trainable (Composed e1 e2) where

  type Dataset (Composed e1 e2) = [(Input e1, Output e2)]  -- only final targets needed

  train dataset (Composed e1 e2) =
    let
      xs          = map fst dataset                    -- raw inputs
      finalTargets = map snd dataset                   -- desired final outputs

      -- 1. Forward pass through first engine
      intermediates = map (infer e1) xs

      -- 2. Train second engine locally (higher-level / later module)
      dataset2 = zip intermediates finalTargets
      e2'      = train dataset2 e2

      -- 3. Train first engine locally, using e2' as critic
      --    In NEAT: this is like using downstream fitness as a signal
      --    In HTM: this is like using higher-region predictions as a local target
      pseudoTargetsForE1 = map (infer e2') intermediates   -- "teacher forcing"
      dataset1 = zip xs pseudoTargetsForE1
      e1'      = train dataset1 e1
    in
      Composed e1' e2'

-- ================================================================
-- Adapter for type conversion
-- ================================================================
newtype Adapter a b = Adapter (a -> b)

instance Engine (Adapter a b) where
  type Input (Adapter a b)  = a
  type Output (Adapter a b) = b
  infer (Adapter f) = f

-- ================================================================
-- Convenient operators
-- ================================================================
infixr 8 ~>>
(~>>) :: (Engine e1, Engine e2, Output e1 ~ Input e2)
      => e1 -> e2 -> Composed e1 e2
e1 ~>> e2 = Composed e1 e2

infixr 8 >~>
(>~>) :: (Engine e1, Engine e2)
      => e1 -> (Output e1 -> Input e2) -> Composed e1 (Adapter (Output e1) (Input e2))
e1 >~> conv = e1 ~>> Adapter conv

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
