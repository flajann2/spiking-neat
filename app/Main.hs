module Main where

import NEAT

main :: IO ()
main = do
  let iconfig = initialConfig
  let (s1, s2, s3, ar) = evalState (
        do 
          ss1 <- nextSequenceNumber 
          ss2 <- nextSequenceNumber 
          ss3 <- nextSequenceNumber
          sar <- seqtest
          return ( ss1, ss2, ss3, sar)
        ) iconfig
 
  putStrLn $ "Hello, Haskell! " ++ show s1
    ++ " and " ++ show s2
    ++ " and " ++ show s3
    ++ " and the array " ++ show ar
    where
      seqtest :: SN [Int64]
      seqtest = do
        ss4 <- nextSequenceNumber
        ss5 <- nextSequenceNumber
        ss6 <- nextSequenceNumber
        return [ss4, ss5, ss6]


