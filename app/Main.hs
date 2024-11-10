module Main where

import NEAT

main :: IO ()
main = do
  let iconfig = initialConfig
  updateConfig iconfig

  s1 <- nextSequenceNumber 
  s2 <- nextSequenceNumber 
  s3 <- nextSequenceNumber
  sar <- seqtest
 
  putStrLn $ "Seq " ++ show s1
    ++ " and " ++ show s2
    ++ " and " ++ show s3
    ++ " and Innov " ++ show ar
    where
      seqtest :: IO [Int64]
      seqtest = do
        ss4 <- nextInnovationNumber
        ss5 <- nextInnovationNumber
        ss6 <- nextInnovationNumber
        return [ss4, ss5, ss6]


