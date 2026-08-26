
{-|
Module      : SDRit
Description : Play with SDR combinators
Copyright   : (c) 2026 Fred Mitchell
License     : MIT
Maintainer  : fred.mitchell@atomlogik.de
-}

module Main (main) where

import Data.SDR
import qualified Data.IntSet as IS

main :: IO ()
main = do
    let my1 = mkSDR 100 (IS.fromRange (10, 25))
    let my2 = mkSDR 100 (IS.fromRange (20, 45))
      
    putStrLn $ bitmapSDR my1
    putStrLn $ bitmapSDR my2
    putStrLn $ bitmapSDR $ my1 /\ my2
    putStrLn $ bitmapSDR $ my1 \/ my2
    putStrLn $ bitmapSDR $ my1 +++ my2
    pure ()
-- >>> mkSDR 100 (IS.fromRange (10, 25))
