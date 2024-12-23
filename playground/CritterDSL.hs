module Main where

import NEAT

default (Double)
main :: IO ()
main = do
  let iconfig = initialConfig
  evalStateT (runSS mainSS) iconfig

mainSS :: SS ()
mainSS = do 
  critter <- genCritter
  liftIO $ pPrint critter
