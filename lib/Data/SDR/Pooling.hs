module Data.SDR.Pooling where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.SDR.Core ( mkSDR, SDR(sdrBits) )
import Data.SDR.Algebras ( union )

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
