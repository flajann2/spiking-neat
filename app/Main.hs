module Main where

import NEAT

main :: IO ()
main = do
  let iconfig = initialConfig
  let (s1, s2, s3) = evalState (
        do 
          s1 <- nextSequenceNumber 
          s2 <- nextSequenceNumber 
          s3 <- nextSequenceNumber 
          return (s1, s2, s3)
        ) iconfig
 
  putStrLn $ "Hello, Haskell! " ++ show s1
    ++ " and " ++ show s2
    ++ " and " ++ show s3
      
