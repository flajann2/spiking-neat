module CritterDSL where

critter :: [Node] -> [Connection] -> SN Critter
critter = undefined

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

critter [ node Regular Input  -- 0
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
