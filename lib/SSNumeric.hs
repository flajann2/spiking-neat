module SSNumeric where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup
import Data.Complex (Complex)
import System.Random ( StdGen, Random(randomR), newStdGen )
import Control.Applicative (liftA2)
import GHC.Float (float2Double)

-- To allow for a generalization of numeric types
-- for example, complex numbers!
--- TODO Delete this block
type SSNum a = ( Num a
               , Show a
               --, Fractional a
               --, Floating a
               , Eq a)

data SSNumeric = SSFloat Float
               | SSDouble Double
               -- | SSComplex Complex a
               deriving Show

instance Eq SSNumeric where
  (SSFloat x) == (SSFloat y) = x == y
  (SSDouble x) == (SSDouble y) = x == y
  
instance Num SSNumeric where
    -- Negation
    negate (SSFloat x)  = SSFloat (negate x)
    negate (SSDouble x) = SSDouble (negate x)

    -- Addition
    (SSFloat x) + (SSFloat y)   = SSFloat (x + y)
    (SSDouble x) + (SSDouble y) = SSDouble (x + y)

    -- Subtraction
    (SSFloat x) - (SSFloat y)   = SSFloat (x - y)
    (SSDouble x) - (SSDouble y) = SSDouble (x - y)

    -- Multiplication
    (SSFloat x) * (SSFloat y)   = SSFloat (x * y)
    (SSDouble x) * (SSDouble y) = SSDouble (x * y)

    -- Absolute value
    abs (SSFloat x)  = SSFloat (abs x)
    abs (SSDouble x) = SSDouble (abs x)

    -- Signum function
    signum (SSFloat x)  = SSFloat (signum x)
    signum (SSDouble x) = SSDouble (signum x)

    -- Conversion from Integer
    fromInteger n = SSFloat  $ fromInteger n
    fromInteger n = SSDouble $ fromInteger n

instance Fractional SSNumeric where
    -- Division
    SSDouble x / SSDouble y = SSDouble (x / y)
    SSFloat x / SSFloat y   = SSFloat (x / y)
    
    -- Mixed division
    SSDouble x / SSFloat y  = SSDouble (x / float2Double y)
    SSFloat x / SSDouble y  = SSDouble (float2Double x / y)

    -- Reciprocal
    recip (SSFloat x)  = SSFloat (recip x)
    recip (SSDouble x) = SSDouble (recip x)

    -- Convert Rational to SSNumeric
    fromRational r = SSFloat $ fromRational r -- or convert to SSDouble if desired

