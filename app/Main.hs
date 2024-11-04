module Main where

import NEAT
import GHC.RTS.Flags (GCFlags(initialStkSize))

main :: IO ()
main = do
  let iconfig = initialConfig
  let (s1, sn) = runState nextSequenceNumber iconfig
  let (s2, sn) = runState nextSequenceNumber sn
  let (s3, sn) = runState nextSequenceNumber sn
  
  putStrLn $ "Hello, Haskell! " ++ show s1
    ++ " and " ++ show s2
    ++ " and " ++ show s3
  where
    seq :: SN Int64
    seq = do
      config <- getConfig
      return $ sequence_number config
      
