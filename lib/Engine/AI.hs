{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Engine.AI where

import Data.Kind (Type)
--- import Data.Functor.Identity (Identity(..))

-- ================================================================
-- Core Inference Engine
-- ================================================================
class Engine engine where
  type Input engine  :: Type
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
  type Input (Composed e1 e2) = Input e1
  type Output (Composed e1 e2) = Output e2
  infer (Composed e1 e2) x = infer e2 (infer e1 x)

-- NEAT / HTM style Trainable instance for composition
-- Each submodule learns locally; downstream module acts as critic/teacher
instance (Trainable e1, Trainable e2,
          Output e1 ~ Input e2,
          Dataset e1 ~ [(Input e1, Output e1)],
          Dataset e2 ~ [(Input e2, Output e2)])
      => Trainable (Composed e1 e2) where
  type Dataset (Composed e1 e2) = [(Input e1, Output e2)] -- only final targets needed

  train dataset (Composed e1 e2) =
    let
      xs = map fst dataset               -- raw inputs
      finalTargets = map snd dataset     -- desired final outputs

      -- 1. Forward pass through first engine
      intermediates = map (infer e1) xs

      -- 2. Train second engine locally (higher-level / later module)
      dataset2 = zip intermediates finalTargets
      e2' = train dataset2 e2

      -- 3. Train first engine locally, using its own previous outputs as pseudo-targets.
      --    (This is the minimal type-correct way to "stabilize" e1 after e2' was updated.
      --     True back-propagation of targets would require an inverse of e2' or a different
      --     training protocol – not possible in this abstract setting.)
      pseudoTargetsForE1 = intermediates
      dataset1 = zip xs pseudoTargetsForE1
      e1' = train dataset1 e1
    in
      Composed e1' e2'

-- ================================================================
-- Adapter for type conversion
-- ================================================================
newtype Adapter a b = Adapter (a -> b)

instance Engine (Adapter a b) where
  type Input (Adapter a b)  = a
  type Output (Adapter a b) = b
  infer :: Adapter a b -> Input (Adapter a b) -> Output (Adapter a b)
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
