{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GHC2021, OverloadedRecordDot #-}

module Main where

import NEAT

import Test.Hspec

main :: IO ()
main = do
  let iconfig = initialConfig
  evalStateT mainSN iconfig

mainSN :: SN ()
mainSN = do
  cfg <- getConfig
  critter <- mkCritter [ node mkRegular             Input  -- 0
                      , node mkRegular             Input  -- 1
                      , node mkRegular             Input  -- 2
                      , node (mkPyramidal 0.5)     Hidden -- 3
                      , node (mkPurkinje  0.5 0.1) Hidden -- 4
                      , node mkInhiborty           Hidden -- 5
                      , node mkRegular             Output -- 6
                      , node mkRegular             Output -- 7
                      ] [ conn 0 3 nxi
                        , conn 1 3 nxi
                        , conn 2 3 nxi
                        , conn 0 4 nxi
                        , conn 1 4 nxi
                        , conn 2 4 nxi
                        , conn 3 6 nxi
                        , conn 4 6 nxi
                        , conn 3 7 nxi
                        , conn 4 7 nxi
                        ]
  _ <- liftIO $ pPrint cfg
  _ <- liftIO $ pPrint critter
  _ <- liftIO $ hspec $ do
    describe "Critter" $ do
      it "has 8 nodes" $ do
        length critter.nodes == 8
    
    describe "Config" $ do
      it "has the initial populaion size set to 100" $ do
        cfg.population_size  `shouldBe` (100 :: Int)
  return ()
