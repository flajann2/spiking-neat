-- LLM-generated

module Engine.Combinators (
                          ) where

import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

-- | A dataset of input-output pairs
type Dataset i o = [(i, o)]

-- | A trained predictor function
type Predictor i o = i -> o

-- | An ML engine: given a dataset, produces a trained predictor.
--   This is deliberately simple and pure (no IO or randomness built-in).
--   If your base engines need randomness (e.g. random forests), you can
--   wrap them to take an explicit seed or use a state monad externally.
newtype Engine i o = Engine
  { trainEngine :: Dataset i o -> Predictor i o }

-- ================================================================
-- Basic combinators
-- ================================================================

-- | Sequential composition = stacking.
--   The first engine transforms the inputs, then the second engine
--   is trained on the predictions of the first (as new "features")
--   paired with the original labels.
stack :: Engine i m -> Engine m o -> Engine i o
stack e1 e2 = Engine $ \dataset ->
  let p1 = trainEngine e1 dataset
      stackedData = [(p1 x, y) | (x, y) <- dataset]
      p2 = trainEngine e2 stackedData
  in \x -> p2 (p1 x)

-- | Parallel averaging (for regression where outputs are Fractional)
average :: Fractional o => [Engine i o] -> Engine i o
average [] = error "average: empty list of engines"
average es = Engine $ \dataset ->
  let predictors = map (`trainEngine` dataset) es
      n = fromIntegral (length predictors)
  in \x -> let preds = map ($ x) predictors
               total = sum preds
           in total / n

-- | Weighted averaging (weights should be positive and ideally sum to 1)
weightedAverage :: Fractional o => [(Double, Engine i o)] -> Engine i o
weightedAverage [] = error "weightedAverage: empty list"
weightedAverage wes = Engine $ \dataset ->
  let trained = [(w, trainEngine e dataset) | (w, e) <- wes]
      totalWeight = sum (map fst trained)
  in \x -> let weightedSum = sum [w * p x | (w, p) <- trained]
           in weightedSum / totalWeight

-- | Majority vote (for classification where outputs are Eq)
majorityVote :: (Eq o, Ord o) => [Engine i o] -> Engine i o
majorityVote [] = error "majorityVote: empty list of engines"
majorityVote es = Engine $ \dataset ->
  let predictors = map (`trainEngine` dataset) es
      mostFrequent xs =
        fst . maximumBy (comparing snd) .
        Map.toList $
        Map.fromListWith (+) [(x, 1) | x <- xs]
  in \x -> mostFrequent (map ($ x) predictors)

-- ================================================================
-- Higher-level ensemble combinators built from the basics
-- ================================================================

-- | Simple uniform ensemble for regression
ensembleAvg :: Fractional o => Int -> Engine i o -> Engine i o
ensembleAvg n base = average (replicate n base)

-- | Simple uniform ensemble for classification
ensembleVote :: (Eq o, Ord o) => Int -> Engine i o -> Engine i o
ensembleVote n base = majorityVote (replicate n base)

-- | Example of a heterogeneous ensemble (mix different base engines)
heterogeneous :: Fractional o => [Engine i o] -> Engine i o
heterogeneous = average
