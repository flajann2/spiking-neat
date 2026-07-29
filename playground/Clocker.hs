{-|
Module      : Playground.Clocker
Description : Minimal driver exercising MonadClock, no domain logic.
Copyright   : (c) 2026 Fred Mitchell
License     : MIT
Maintainer  : fred.mitchell@atomlogik.de
-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Engine.Clock

-- | One tick, printed. Works in any MonadClock that's also MonadIO.
--   Also demonstrates how to get the dt and current time from the MonadClock.
tickAndPrint :: (MonadClock m, MonadIO m) => m ()
tickAndPrint = do
  dt <- tick
  t  <- currentTime
  liftIO $ putStrLn $ "dt = " <> show dt <> "s, elapsed = " <> show t <> "s"

-- | Deterministic demo: fixed dt, prints live now that SimClock is MonadIO.
runSimDemo :: Float -> Int -> IO ()
runSimDemo dt n = do
  total <- clockS tickAndPrint dt n
  putStrLn $ "Total simulated time: " ++ show total ++ "s"

-- | Live demo: real wall-clock ticks, printed as they happen.
-- Sleeps briefly between ticks just so dt is visibly nonzero.
-- Also demonstates the use of forever.
runRealDemoForever :: Float -> IO ()
runRealDemoForever dt = do
  clockR tickAndPrint dt

main :: IO ()
main = do
  putStrLn "-- SimClock demo (fixed dt = 0.1s) --"
  runSimDemo dt n

  putStrLn ""
  putStrLn "-- RealClock demo (wall-clock, ~200ms steps) --"
  runRealDemoForever dt
  where
    dt = 0.1
    n  = 25
