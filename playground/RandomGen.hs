module Main where

import System.Random ( StdGen
                     , Random(randomRs, randomR)
                     , mkStdGen
                     , newStdGen )

import SSMonad ( evalStateT
               , initialConfig
               , nextRandom
               , SS(..) )

main :: IO ()
main = do
    let seed = 42                      -- Set your seed here
    let gen = mkStdGen seed            -- Create the generator with the seed
    
    -- Generate a single random number in the range [0.0, 1.0]
    let (randomNum1, newGen) = randomR (0.0, 1.0) gen :: (Double, StdGen)
    print $ "repeatable random " ++ show randomNum1                   -- Print the first random number
    
    -- Generate multiple random numbers in the range [0.0, 1.0]
    let randomNumbers = take 10 (randomRs (0.0, 1.0) newGen :: [Double])
    print randomNumbers                 -- Print the list of random numbers

    genr <- newStdGen  -- Get a new true random generator
    let (randomNum, newGen2) = randomR (0.0, 1.0) genr :: (Double, StdGen)  -- Generate a random Double in the range [0.0, 1.0]
    print $ "true random " ++ show randomNum  -- Print the generated random number

    -- Generate true multiple random numbers in the range [0.0, 1.0]
    let randomNumbers' = take 4 (randomRs (0.0, 1.0) newGen2 :: [Double])
    print randomNumbers'
    
    -- Generate true multiple random numbers in the range [0.0, 1.0]
    let rinf = (randomRs (0.0, 1.0) newGen2 :: [Double])
    print $ take 100 rinf

    let iconfig = initialConfig
    (   r1
      , r2
      , r3
      , r4) <- evalStateT ( do
                              rnum1 <- runSS $ nextRandom (0   :: Int,    10  :: Int)
                              rnum2 <- runSS $ nextRandom (0.0 :: Double, 1.0 :: Double)
                              rnum3 <- runSS $ nextRandom (0.0 :: Double, 1.0 :: Double)
                              rnum4 <- runSS $ nextRandom (0.0 :: Double, 1.0 :: Double)
                              return (rnum1, rnum2, rnum3, rnum4)
                          ) iconfig
    putStrLn $    "r1: " ++ show r1
             ++ "\nr2: " ++ show r2
             ++ "\nr3: " ++ show r3
             ++ "\nr4: " ++ show r4
