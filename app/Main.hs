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
 
  putStrLn $ "Seq " ++ show s1
    ++ " and " ++ show s2
    ++ " and " ++ show s3
    ++ " and Innov " ++ show ar
    where
      seqtest :: SN [Int64]
      seqtest = do
        ss4 <- nextInnovationNumber
        ss5 <- nextInnovationNumber
        ss6 <- nextInnovationNumber
        return [ss4, ss5, ss6]


