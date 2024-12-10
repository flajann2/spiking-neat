module Main where

import NEAT

default (Double)

main :: IO ()
main = do
  let iconfig = initialConfig
  _ <- evalStateT mainSN iconfig
  return ()

mainSN :: SN Int 
mainSN = do 
  s1 <- nextSequenceNumber 
  s2 <- nextSequenceNumber 
  s3 <- nextSequenceNumber
  ar <- seqtest
  cfg <- getConfig
  liftIO $ putStrLn $ "from mainSN, " ++ show s1
    ++ " and " ++ show s2
    ++ " and " ++ show s3
    ++ " and Innov " ++ show ar
    ++ "\n and the config: " ++ show cfg
  return 0
    where
   --   seqtest :: SN [Int]
      seqtest = do
        ss4 <- nextInnovationNumber
        ss5 <- nextInnovationNumber
        ss6 <- nextInnovationNumber
        return [ss4, ss5, ss6]
-- >>> putStrLn "Hello\nWorld"
