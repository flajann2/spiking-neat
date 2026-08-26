module Data.SDR.Algebras where

import qualified Data.IntSet as IS
import Numeric (showFFloat)
import Data.SDR.Core

--------------------------------------------------------------------------------
-- Set-algebraic combinators (HTM primitives)
--------------------------------------------------------------------------------

-- | Number of bits active in both SDRs. The core similarity primitive
-- HTM builds everything else on top of.
overlap :: SDR -> SDR -> Int
overlap a b = requireSameWidth a b $
  IS.size (IS.intersection (sdrBits a) (sdrBits b))

union :: SDR -> SDR -> SDR
union a b = requireSameWidth a b $
  SDR (sdrWidth a) (IS.union (sdrBits a) (sdrBits b))

intersectSDR :: SDR -> SDR -> SDR
intersectSDR a b = requireSameWidth a b $
  SDR (sdrWidth a) (IS.intersection (sdrBits a) (sdrBits b))

difference :: SDR -> SDR -> SDR
difference a b = requireSameWidth a b $
  SDR (sdrWidth a) (IS.difference (sdrBits a) (sdrBits b))

-- | Fraction of bits active, typically ~2% for well-formed SDRs.
sparsity :: SDR -> Double
sparsity s = fromIntegral (IS.size (sdrBits s)) / fromIntegral (sdrWidth s)

hammingDistance :: SDR -> SDR -> Int
hammingDistance a b = requireSameWidth a b $
  IS.size (IS.union (IS.difference ba bb) (IS.difference bb ba))
  where ba = sdrBits a; bb = sdrBits b

jaccard :: SDR -> SDR -> Double
jaccard a b
  | IS.null (IS.union ba bb) = 0
  | otherwise = fromIntegral (IS.size (IS.intersection ba bb))
              / fromIntegral (IS.size (IS.union ba bb))
  where ba = sdrBits a; bb = sdrBits b

-- | Concatenate two SDRs, producing one SDR whose width is the sum of
-- both widths. Bit indices from the second SDR are offset by the
-- width of the first, so no collision is possible regardless of what
-- was active in either input.
--
-- Unlike 'union'/'intersectSDR'/'difference', concatenation does NOT
-- require matching widths -- that's the entire point of the operation
-- (it's how multi-field encoders compose fields of different widths
-- into one wider SDR). Don't route this through 'requireSameWidth'.
concatSDR :: SDR -> SDR -> SDR
concatSDR a b = SDR (sdrWidth a + sdrWidth b)
                     (IS.union (sdrBits a) (IS.map (+ sdrWidth a) (sdrBits b)))


-- | Pretty printing an SDR
--   This is mainly for debugging purposes
prettySDR :: SDR -> String
prettySDR s = mconcat
  [ "SDR<w=", show (sdrWidth s)
  , ", n=", show (IS.size (sdrBits s))
  , ", sparsity=", showFFloat (Just 3) (sparsity s) ""
  , "> ", show (take 12 (IS.toList (sdrBits s)))
  , if IS.size (sdrBits s) > 12 then "..." else ""
  ]

-- | Pretty printing an SDR bitmap
--   This is mainly for debugging purposes, and for very large bitmaps
--   it may become ungangly.
bitmapSDR :: SDR -> String
bitmapSDR s = [ if IS.member i bits then '#' else '.' | i <- [0 .. sdrWidth s - 1] ]
  where bits = sdrBits s
