module CritterDSL where

import NEAT

main :: IO ()
main = do
  cfg <- getConfig
  critter <- evalStateT (
    do 
      critter' <- mkCritter [ node mkRegular Input  -- 0
                           , node mkRegular Input  -- 1
                           , node mkRegular Input  -- 2
                           , node mkRegular Input  -- 3
                           , node mkRegular Input  -- 4
                           , node mkRegular Output -- 5
                           , node mkRegular Output -- 6
                           , node mkRegular Output -- 7
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
      return critter'
    ) cfg
  putStrLn critter
  
{-
critter nodes
  <$> node Regular Input  -- 0
  <*> node Regular Input  -- 1
  <*> node Regular Input  -- 2
  <*> node Regular Input  -- 3
  <*> node Regular Input  -- 4
  <*> node Regular Output -- 5
  <*> node Regular Output -- 6
  <*> node Regular Output -- 7
  connections
  <$> conn 0 6
  <*> conn 1 6
  <*> conn 2 6
  <*> conn 3 7
  <*> conn 2 7
  <*> conn 4 5
  <*> conn 3 5
  <*> conn 0 5
  <*> conn 0 7
  <*> conn 4 7
-}
