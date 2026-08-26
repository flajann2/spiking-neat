module Data.SDR.Infix (
  -- * Infix operators
    (\/)
  , (/\)
  , (\\)
  , (<&>.)
  , (<~>)
  , (<->)
  , (+++)
  ) where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Numeric (showFFloat)

--------------------------------------------------------------------------------
-- Infix representation of set algebraic combinators
--------------------------------------------------------------------------------

infixl 6 \/    -- union            (like +)
infixl 7 /\    -- intersectSDR     (like *, binds tighter than \/)
infixl 6 \\    -- difference       (matches Data.List/Data.Set convention)
infix  4 <&>.  -- overlap          (Int) -- avoid <&> (Functor, Data.Functor)
infix  4 <~>   -- jaccard          (Double, similarity)
infix  4 <->   -- hammingDistance  (Int, distance)
infixr 5 +++   -- Concatenate

(\/) :: SDR -> SDR -> SDR
(\/) = union

(/\) :: SDR -> SDR -> SDR
(/\) = intersectSDR

(\\) :: SDR -> SDR -> SDR
(\\) = difference

(<&>.) :: SDR -> SDR -> Int
(<&>.) = overlap

(<~>) :: SDR -> SDR -> Double
(<~>) = jaccard

(<->) :: SDR -> SDR -> Int
(<->) = hammingDistance

(+++) :: SDR -> SDR -> SDR
(+++) = concatSDR

-- | Wrapper providing a Monoid instance for concatenation. Unlike
-- UnionSDR, concatenation has a genuine identity element: a width-0
-- empty SDR, since concatenating anything with a width-0 SDR just
-- returns the original SDR unchanged (0 + n = n, and there are no
-- bits to offset-collide with).
newtype ConcatSDR = ConcatSDR { getConcatSDR :: SDR }
  deriving (Eq, Show)

instance Semigroup ConcatSDR where
  ConcatSDR a <> ConcatSDR b = ConcatSDR (concatSDR a b)

instance Monoid ConcatSDR where
  mempty = ConcatSDR (emptySDR 0)
