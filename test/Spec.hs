{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec where

-- import NEAT

import Test.Hspec.Discover ( Monad(return), IO, hspec, Spec )
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (24 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
{-
  crit <- mkCritter [ node Regular Input  -- 0
                   , node Regular Input  -- 1
                   , node Regular Input  -- 2
                   , node Regular Input  -- 3
                   , node Regular Input  -- 4
                   , node Regular Output -- 5
                   , node Regular Output -- 6
                   , node Regular Output -- 7
                   ] [ conn 0 6
                     , conn 1 6
                     , conn 2 6
                     , conn 3 7
                     , conn 2 7
                     , conn 4 5
                     , conn 3 5
                     , conn 0 5
                     , conn 0 7
                     , conn 4 7
                     ]
-}
