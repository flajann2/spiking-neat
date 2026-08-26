module Data.SDR.Slicing where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Numeric (showFFloat)
import Data.SDR.Core

--------------------------------------------------------------------------------
-- Slicing / splitting (inverse of concatenation)
--------------------------------------------------------------------------------

-- | Extract the contiguous sub-range [lo, lo+w) of an SDR as a new
-- SDR of width w. Bit indices are re-based relative to lo, so a bit
-- at absolute position (lo + k) becomes bit k in the result -- this
-- is what makes it the true inverse of concatSDR/(+++): slicing a
-- concatenated SDR back apart recovers the original pieces exactly.
sliceSDR :: Int -> Int -> SDR -> SDR
sliceSDR lo w s
  | lo < 0 || w < 0 || lo + w > sdrWidth s =
      error "SDR.sliceSDR: range out of bounds"
  | otherwise =
      mkSDRUnsafe w (IS.map (subtract lo) (IS.filter inRange (sdrBits s)))
  where
    inRange i = i >= lo && i < lo + w

-- | Split an SDR into two pieces at bit offset 'at'. The first result
-- has width 'at', the second has width (sdrWidth s - at). Satisfies
-- @uncurry (+++) (splitSDR at s) == s@ for any valid 'at'.
splitSDR :: Int -> SDR -> (SDR, SDR)
splitSDR at s = (sliceSDR 0 at s, sliceSDR at (sdrWidth s - at) s)

-- | Split an SDR into consecutive pieces of the given widths, which
-- must sum to the SDR's total width. Exact inverse of 'encodeConcat'
-- and 'mconcat . map ConcatSDR' -- useful for pulling a multi-field
-- encoded SDR back apart into its original per-field pieces.
splitSDRBy :: [Int] -> SDR -> [SDR]
splitSDRBy widths s
  | sum widths /= sdrWidth s =
      error "SDR.splitSDRBy: widths don't sum to SDR width"
  | otherwise = go 0 widths
  where
    go _ []       = []
    go off (w:ws) = sliceSDR off w s : go (off + w) ws

-- | Zero out all bits outside [lo, lo+w), keeping the SDR's original
-- width unchanged. Unlike 'sliceSDR', this does NOT re-index bits or
-- shrink the SDR -- the result stays directly comparable (same width)
-- to the original via overlap/union/etc.
restrictSDR :: Int -> Int -> SDR -> SDR
restrictSDR lo w s = mkSDRUnsafe (sdrWidth s) (IS.filter inRange (sdrBits s))
  where
    inRange i = i >= lo && i < lo + w
