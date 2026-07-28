{-|
Module      : Playground.Clocker
Description : Minimal driver exercising MonadClock, no domain logic.
Copyright   : (c) 2026 Fred Mitchell
License     : MIT
Maintainer  : fred.mitchell@atomlogik.de
-}

module Main (main) where

import Control.Monad (replicateM_
                     , forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (threadDelay)

import Engine.Clock

-- | One tick, printed. Works in any MonadClock that's also MonadIO.
tickAndPrint :: (MonadClock m, MonadIO m) => m ()
tickAndPrint = do
  dt <- tick
  t  <- currentTime
  liftIO $ putStrLn $ "dt = " ++ show dt ++ "s, elapsed = " ++ show t ++ "s"

-- | Deterministic demo: fixed dt, prints live now that SimClock is MonadIO.
runSimDemo :: Float -> Int -> IO ()
runSimDemo dt n = do
  (_, total) <- runSimClock dt (replicateM_ n tickAndPrint)
  putStrLn $ "Total simulated time: " ++ show total ++ "s"

-- | Live demo: real wall-clock ticks, printed as they happen.
-- Sleeps briefly between ticks just so dt is visibly nonzero.
-- Also demonstates the use of forever.
runRealDemoForever :: IO ()
runRealDemoForever = do
  env <- newRealClock
  runRealClock env $ forever $ do
    liftIO $ threadDelay 200000  -- 200ms, just to make dt visible
    tickAndPrint

main :: IO ()
main = do
  putStrLn "-- SimClock demo (fixed dt = 0.1s) --"
  runSimDemo 0.1 25

  putStrLn ""
  putStrLn "-- RealClock demo (wall-clock, ~200ms steps) --"
  runRealDemoForever
