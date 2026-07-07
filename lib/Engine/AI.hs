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

-- ================================================================
-- Supertype that encompasses Engine and Adapter
-- ================================================================
class Wandler w where
    type Input w :: Type
    type Output w :: Type

-- ================================================================
-- Core Inference Engine
-- ================================================================
class Wandler engine => Engine engine where
    infer :: engine -> Input engine -> Output engine

-- Batch inference (works on any Functor)
inferMany :: (Functor f, Engine e) => e -> f (Input e) -> f (Output e)
inferMany engine = fmap (infer engine)

-- ================================================================
-- Trainable Engines (Local / Modular Learning)
-- ================================================================
class Engine engine => Trainable engine where
    type Dataset engine :: Type
    train :: Dataset engine -> engine -> engine

-- ================================================================
-- Composition
-- ================================================================
data Composed e1 e2 = Composed e1 e2

instance (Engine e1, Engine e2, Output e1 ~ Input e2)
      => Wandler (Composed e1 e2) where
    type Input  (Composed e1 e2) = Input e1
    type Output (Composed e1 e2) = Output e2

instance (Engine e1, Engine e2, Output e1 ~ Input e2)
      => Engine (Composed e1 e2) where
    infer (Composed e1 e2) x = infer e2 (infer e1 x)

-- ================================================================
-- Adapter for type conversion
-- ================================================================
newtype Adapter a b = Adapter (a -> b)

instance Wandler (Adapter a b) where
    type Input  (Adapter a b) = a
    type Output (Adapter a b) = b

instance Engine (Adapter a b) where
    infer (Adapter f) = f

-- ================================================================
-- Trainable instance for Composed (NEAT/HTM style)
-- ================================================================
instance (Trainable e1, Trainable e2,
          Output e1 ~ Input e2,
          Dataset e1 ~ [(Input e1, Output e1)],
          Dataset e2 ~ [(Input e2, Output e2)])
      => Trainable (Composed e1 e2) where

    type Dataset (Composed e1 e2) = [(Input e1, Output e2)]

    train dataset (Composed e1 e2) =
        let
            xs            = map fst dataset
            finalTargets  = map snd dataset
            intermediates = map (infer e1) xs
            dataset2      = zip intermediates finalTargets
            e2'           = train dataset2 e2
            pseudoTargetsForE1 = intermediates
            dataset1      = zip xs pseudoTargetsForE1
            e1'           = train dataset1 e1
        in
            Composed e1' e2'

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
