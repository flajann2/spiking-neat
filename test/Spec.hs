{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Spec where


import Test.Hspec.Discover ( Monad(return), IO, hspec, Spec )
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = do
  putStrLn "high time to write some tests, buddy."

  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
