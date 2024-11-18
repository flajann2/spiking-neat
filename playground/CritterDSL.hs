module Main where

import NEAT

main :: IO ()
main = do
  let iconfig = initialConfig
  evalStateT mainSN iconfig

mainSN :: SN ()
mainSN = do 
  critter <- mkCritter [  node mkRegular Input  -- 0
                       , node mkRegular Input  -- 1
                       , node mkRegular Input  -- 2
                       , node mkRegular Input  -- 3
                       , node mkRegular Input  -- 4
                       , node mkRegular Output -- 5
                       , node mkRegular Output -- 6
                       , node mkRegular Output -- 7
                       ] [ conn 0 6 nxi
                         , conn 1 6 nxi
                         , conn 2 6 nxi
                         , conn 3 7 nxi
                         , conn 2 7 nxi
                         , conn 4 5 nxi
                         , conn 3 5 nxi
                         , conn 0 5 nxi
                         , conn 0 7 nxi
                         , conn 4 7 nxi
                         ]
  liftIO $ pPrint critter
