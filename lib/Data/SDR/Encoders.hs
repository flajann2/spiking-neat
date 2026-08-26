module Data.SDR.Encoders where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Numeric (showFFloat)

--------------------------------------------------------------------------------
-- Encoders
--------------------------------------------------------------------------------

data EncoderParams = EncoderParams
  { epWidth  :: !Int  -- ^ total output width, n
  , epActive :: !Int  -- ^ number of active bits per encoding, w
  } deriving (Eq, Show)

class Encodable a where
  encode :: EncoderParams -> a -> SDR

-- | Scalar encoder: a contiguous window of 'epActive' bits sliding
-- across 'epWidth' buckets by value. Nearby values overlap heavily;
-- distant values share no bits. First real instance targets the
-- crypto price feed proof-of-concept.
instance Encodable Double where
  encode (EncoderParams w active) x =
    let bucket = clampedBucketIndex w active x
    in mkSDR w (IS.fromList [bucket .. bucket + active - 1])

-- | Map a value into [0, w - active] via caller-supplied min/max/
-- resolution logic. Placeholder: wire up real domain bounds
-- (e.g. price min/max with a resolution parameter) before use --
-- this stub just clamps a raw value into range assuming x is already
-- normalized to [0, 1].
clampedBucketIndex :: Int -> Int -> Double -> Int
clampedBucketIndex w active x =
  let maxBucket = w - active
      raw       = round (x * fromIntegral maxBucket)
  in max 0 (min maxBucket raw)

-- | Concatenate multiple sub-SDRs into one wider SDR, offsetting each
-- sub-range's bit indices by the cumulative width of the fields
-- before it. This is how multi-field encoders (htm.core style) are
-- built: encode each field independently, then concatenate.
encodeConcat :: [SDR] -> SDR
encodeConcat sdrs = SDR totalWidth allBits
  where
    widths     = map sdrWidth sdrs
    offsets    = scanl (+) 0 widths
    totalWidth = sum widths
    allBits    = IS.unions
      [ IS.map (+ off) (sdrBits s)
      | (s, off) <- zip sdrs offsets
      ]
