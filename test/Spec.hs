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
      head [23 ..] `shouldBe` (24 :: Int) -- this should fail!

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
