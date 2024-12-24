{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Genetics.Critters where

import SSMonad 
import SSNumeric
import Genetics.Genes
import Control.Monad (mapM)
import GHC.Cmm.Utils (mkRODataLits)
import GHC.Builtin.Uniques (mkRegClassUnique)

data Critter = Critter { nodes          :: [Node]
                       , inputs         :: [Int] -- indices of the input nodes
                       , outputs        :: [Int] -- indices of the output nodes
                       , hidden         :: [Int] -- indices of the hidden nodes
                       , connections    :: [Connection]
                       , number_inputs  :: Int
                       , number_outputs :: Int
                       } deriving Show

class Eval where
  ecritter :: [a] -> SS [a]
  epopulation :: [a] -> SS [[a]]

node :: NType -> Role -> Node
node nt r = Node { ntype = nt
                 , role = r }

rwei :: SS SSNumeric
rwei = do
  r <- nextRandom (0.0, 5.0)
  pure $ SSDouble r
  
conn ::  Int -> Int -> SS Int -> SS Connection
conn in' out' innov = do
  innov' <- innov
  rwei' <- rwei
  return $ Connection { innovation = innov'
                      , node_in    = in'
                      , node_out   = out'
                      , weight     = rwei'
                      , enabled    = True                             
                      }

mkCritter :: [Node] -> [SS Connection] -> SS Critter
mkCritter ns cs = do
  cfg <- getConfig
  cs' <- mapM id cs
  let crit = Critter { nodes          = ns
                     , connections    = cs'
                     , number_inputs  = cfg.num_inputs
                     , number_outputs = cfg.num_outputs
                     , inputs         = findInputs
                     , outputs        = findOutputs
                     , hidden         = findHidden
                     }
  return crit
    where
      findInputs :: [Int]
      findInputs = [i | (n, i) <- zip ns [0..], n.role == Input] 
      
      findOutputs :: [Int]
      findOutputs = [i | (n, i) <- zip ns [0..], n.role == Output]

      findHidden :: [Int]
      findHidden = [i | (n, i) <- zip ns [0..], n.role == Hidden]

genCritter :: SS Critter 
genCritter = do
  cfg <- getConfig
  let conns = genconn cfg.num_inputs cfg.num_outputs
  _ <- liftIO $ putStrLn $ show cfg.num_inputs
  _ <- liftIO $ putStrLn $ show cfg.num_outputs
  _ <- liftIO $ putStrLn $ show $ length conns
  crit <- mkCritter    ((nodes (cfg.num_inputs)  Input [])
                   ++ (nodes (cfg.num_outputs) Output [])) conns
                   -- $ genconn cfg.num_inputs cfg.num_outputs
  return crit
  where
    nodes :: Int -> Role -> [Node] -> [Node]
    nodes 0 _ ns = ns
    nodes cnt r ns = nodes (cnt - 1) r $ (node mkRegular r) : ns

    genconn :: Int -> Int -> [SS Connection]
    genconn ins outs = [conn i j nxi | i <- [0..(ins-1)], j <- [ins..(ins+outs-1)]]
