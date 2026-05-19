{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Combinators where

import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

-- | A dataset of input-output pairs
type Dataset i o = [(i, o)]

-- | A trained predictor function
type Predictor i o = i -> o

-- | An ML engine: given a dataset, produces a trained predictor.
newtype Engine i o = Engine
  { trainEngine :: Dataset i o -> Predictor i o }

-- ================================================================
-- Basic combinators
-- ================================================================

-- | Sequential composition (stacking).
-- The first engine is trained on the original data, its predictions become
-- the new features for the second engine.
--
-- Note: This requires that the intermediate representation `m` has the same
-- type as the final label `o` (because we have no ground-truth labels for `m`).
-- If you need a true feature-transform + meta-learner stack with different types,
-- you will need an unsupervised/pretrained first stage or a different design.
stack :: (m ~ o) => Engine i m -> Engine m o -> Engine i o
stack e1 e2 = Engine $ \dataset ->
  let p1 = trainEngine e1 dataset                     -- now type-checks
      stackedData = [(p1 x, y) | (x, y) <- dataset]
      p2 = trainEngine e2 stackedData
  in \x -> p2 (p1 x)


-- | Parallel averaging (for regression)
average :: Fractional o => [Engine i o] -> Engine i o
average [] = error "average: empty list of engines"
average es = Engine $ \dataset ->
  let predictors = map (`trainEngine` dataset) es
      n = fromIntegral (length predictors)
  in \x -> let preds = map ($ x) predictors
           in sum preds / n


-- | Weighted averaging (weights should be positive)
weightedAverage :: Fractional o => [(Double, Engine i o)] -> Engine i o
weightedAverage [] = error "weightedAverage: empty list"
weightedAverage wes = Engine $ \dataset ->
  let trained = [(w, trainEngine e dataset) | (w, e) <- wes]
      totalWeight = sum (map fst trained)
  in \x ->
       let weightedSum = sum [realToFrac w * p x | (w, p) <- trained]
       in weightedSum / realToFrac totalWeight


-- | Majority vote for classification
majorityVote :: (Eq o, Ord o) => [Engine i o] -> Engine i o
majorityVote [] = error "majorityVote: empty list of engines"
majorityVote es = Engine $ \dataset ->
  let predictors = map (`trainEngine` dataset) es
      mostFrequent xs =
        fst . maximumBy (comparing snd) .
        Map.toList $ Map.fromListWith (+) [(x, 1) | x <- xs]
  in \x -> mostFrequent (map ($ x) predictors)


-- ================================================================
-- Higher-level ensemble combinators
-- ================================================================

ensembleAvg :: Fractional o => Int -> Engine i o -> Engine i o
ensembleAvg n base = average (replicate n base)

ensembleVote :: (Eq o, Ord o) => Int -> Engine i o -> Engine i o
ensembleVote n base = majorityVote (replicate n base)

heterogeneous :: Fractional o => [Engine i o] -> Engine i o
heterogeneous = average
