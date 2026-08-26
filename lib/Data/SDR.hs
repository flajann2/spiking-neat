{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.SDR
--
-- Sparse Distributed Representation core: the universal input/output
-- interface for data entering the Rhayader spiking network.
--
-- Design notes:
--   * SDR carries its width explicitly (not phantom-typed) so that
--     combinators can do a cheap runtime dimensionality check rather
--     than silently operating across mismatched spaces.
--   * Combinators (overlap, union, intersectSDR, difference, ...) are
--     total but 'error' on width mismatch -- a mismatch here is a
--     programmer bug (wrong encoder wired to wrong pooler), not a
--     recoverable runtime condition. If SDRs ever get constructed from
--     untrusted/external width parameters (e.g. deserialized genomes),
--     switch the boundary functions to Either SDRError SDR.
--   * Active-bit indices double as input-neuron IDs when bridging into
--     Spiking NEAT: if the genome's input neuron ID space equals the
--     SDR's bit-index space (0 .. width-1), an active bit *is* an
--     input neuron ID, with no translation layer required.

module Data.SDR
  ( -- * Core type
    SDR(sdrWidth, sdrBits)
  , mkSDR
  , mkSDRUnsafe
  , emptySDR

    -- * Set-algebraic combinators (HTM primitives)
  , overlap
  , union
  , intersectSDR
  , difference
  , sparsity
  , hammingDistance
  , jaccard

    -- * Union pooling (temporal persistence via monoidal OR)
  , UnionSDR(..)

    -- * Spatial pooling: k-winners-take-all
  , kWTA
  , applyBoost

    -- * Encoders
  , Encodable(..)
  , EncoderParams(..)
  , encodeConcat
  , concatSDR
  , ConcatSDR(..)

    -- * Slicing / splitting
  , sliceSDR
  , splitSDR
  , splitSDRBy
  , restrictSDR

    -- * Temporal memory: anomaly scoring
  , anomalyScore
  , rawAnomalyScore

    -- * Bridge into Spiking NEAT
  , sdrToInputCurrent
  , applyExternalCurrent
  , Neuron(..)

  , IntSet
  , prettySDR
  , bitmapSDR

    -- * Submodules
  , module Data.SDR.Infix
  , module Data.SDR.Algebras
  , module Data.SDR.Encoders
  , module Data.SDR.Slicing
  ) where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Numeric (showFFloat)
import Data.SDR.Infix
import Data.SDR.Encoders
import Data.SDR.Slicing
import Data.SDR.Algebras

--------------------------------------------------------------------------------
-- Core type
--------------------------------------------------------------------------------

data SDR = SDR
  { sdrWidth :: {-# UNPACK #-} !Int   -- ^ dimensionality, n
  , sdrBits  :: !IntSet               -- ^ active bit indices, subset of [0, sdrWidth)
  } deriving (Eq, Ord, Show)

-- | Construct an SDR, checking that all active bits fall in range.
-- Errors on out-of-range bits. Safe for boundaries where input isn't
-- already known-good (e.g. deserialization, encoder output).
mkSDR :: Int -> IntSet -> SDR
mkSDR w bits
  | IS.null bits = SDR w bits
  | IS.findMin bits >= 0 && IS.findMax bits < w = SDR w bits
  | otherwise = error "SDR.mkSDR: bit index out of range"

-- | Construct an SDR without the range check, for hot paths where the
-- caller has already validated invariants upstream.
mkSDRUnsafe :: Int -> IntSet -> SDR
mkSDRUnsafe = SDR

emptySDR :: Int -> SDR
emptySDR w = SDR w IS.empty

requireSameWidth :: SDR -> SDR -> a -> a
requireSameWidth a b x
  | sdrWidth a == sdrWidth b = x
  | otherwise = error "SDR: width mismatch"

--------------------------------------------------------------------------------
-- Union pooling
--------------------------------------------------------------------------------

-- | Wrapper providing a Semigroup instance for OR-based temporal
-- pooling (accumulate a persistent trace of recently active columns).
-- Kept separate from SDR itself: SDR has no principled 'mempty' since
-- an empty SDR still needs a width, so we don't pretend Monoid SDR
-- makes sense without one.
newtype UnionSDR = UnionSDR { getUnionSDR :: SDR }
  deriving (Eq, Show)

instance Semigroup UnionSDR where
  UnionSDR a <> UnionSDR b = UnionSDR (union a b)

--------------------------------------------------------------------------------
-- Spatial pooling: k-winners-take-all
--------------------------------------------------------------------------------

-- | Select the top-k scoring columns as the active SDR. This is the
-- operation that turns a scored column population (raw overlap scores,
-- or boosted scores) into an actual sparse representation.
kWTA :: Int -> Int -> IntMap Double -> SDR
kWTA width k scores =
  mkSDR width . IS.fromList . map fst . take k
  $ sortOn (Down . snd) (IM.toList scores)

-- | Homeostatic boosting: multiply raw overlap scores by a per-column
-- boost factor before kWTA selects winners, so chronically
-- under-active columns get a fairer shot.
applyBoost :: IntMap Double -> IntMap Double -> IntMap Double
applyBoost boosts overlaps = IM.intersectionWith (*) boosts overlaps

--------------------------------------------------------------------------------
-- Temporal memory: anomaly scoring
--------------------------------------------------------------------------------

-- | Raw anomaly score: fraction of the actual SDR's active bits that
-- were NOT predicted. 0 = fully predicted (unsurprising), 1 = nothing
-- predicted (completely novel).
rawAnomalyScore :: SDR    -- ^ predicted SDR (from prior timestep's TM prediction)
                -> SDR    -- ^ actual SDR (this timestep's SP output)
                -> Double
rawAnomalyScore predicted actual
  | activeCount == 0 = 0  -- nothing active this step: define as no anomaly
  | otherwise = fromIntegral unpredicted / fromIntegral activeCount
  where
    activeCount = IS.size (sdrBits actual)
    unpredicted = IS.size (IS.difference (sdrBits actual) (sdrBits predicted))

-- | Anomaly score with an exponential moving average for a smoothed
-- signal, since raw per-step anomaly is typically noisy.
anomalyScore :: Double        -- ^ smoothing factor alpha, in (0, 1]
             -> Double        -- ^ previous smoothed anomaly
             -> SDR           -- ^ predicted SDR
             -> SDR           -- ^ actual SDR
             -> Double        -- ^ new smoothed anomaly
anomalyScore alpha prevSmoothed predicted actual =
  alpha * raw + (1 - alpha) * prevSmoothed
  where raw = rawAnomalyScore predicted actual

--------------------------------------------------------------------------------
-- Pretty printing for debugging, etc.
--------------------------------------------------------------------------------

prettySDR :: SDR -> String
prettySDR s = mconcat
  [ "SDR<w=", show (sdrWidth s)
  , ", n=", show (IS.size (sdrBits s))
  , ", sparsity=", showFFloat (Just 3) (sparsity s) ""
  , "> ", show (take 12 (IS.toList (sdrBits s)))
  , if IS.size (sdrBits s) > 12 then "..." else ""
  ]

bitmapSDR :: SDR -> String
bitmapSDR s = [ if IS.member i bits then '#' else '.' | i <- [0 .. sdrWidth s - 1] ]
  where bits = sdrBits s

--------------------------------------------------------------------------------
-- Bridge into Spiking NEAT
--------------------------------------------------------------------------------

-- | Minimal neuron shape for illustrating the bridge -- replace with
-- (or import) the real Izhikevich neuron record from the network
-- simulation module; this is here only so the file is self-contained
-- and compiles standalone.
data Neuron = Neuron
  { neuronCurrent :: !Double
  -- , neuronV, neuronU, etc. live in the real Rhayader.Neuron module
  } deriving (Eq, Show)

-- | Translate an SDR's active bits into external input current, one
-- entry per active bit, keyed by bit index. Works directly as input
-- neuron IDs when the genome's input address space equals the SDR's
-- bit-index space -- no translation table required.
--
-- This is the simplest translation (constant current per active bit,
-- for the duration the SDR is "presented"); swap in Poisson spike
-- trains per active bit later without touching callers.
sdrToInputCurrent :: Double -> SDR -> IntMap Double
sdrToInputCurrent current sdr =
  IM.fromList [ (i, current) | i <- IS.toList (sdrBits sdr) ]

-- | Fold external input current into a population of neurons keyed by
-- ID, adding to whatever current a neuron already has accumulated
-- this step. Neurons with no incoming external current pass through
-- untouched.
applyExternalCurrent :: IntMap Double -> IntMap Neuron -> IntMap Neuron
applyExternalCurrent ext = IM.mergeWithKey
  (\_ i n -> Just n { neuronCurrent = neuronCurrent n + i })
  (const IM.empty)
  id
  ext
