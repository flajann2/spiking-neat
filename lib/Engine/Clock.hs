{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes #-}

{-|
Module      : Engine.Clock
Description : Clock for all engines that need it.
Copyright   : (c) 2026 Fred Mitchell
License     : MIT
Maintainer  : fred.mitchell@atomlogik.de
TODO Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Engine.Clock ( MonadClock(..)
                    , SimClock
                    , RealClock
                    , clockS
                    , clockR
                    ) where

import Control.Monad (replicateM_
                     , forever)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.IORef
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

-- | Anything that can hand out a notion of "current time" and "step size".
class Monad m => MonadClock m where
  -- | Advance the clock by one tick, returning the elapsed dt (seconds).
  tick :: m Float
  -- | Total simulation time elapsed so far (seconds).
  currentTime :: m Float

-- --------------------------------------------------------------------------
-- SimClock: deterministic, fixed-step clock for testing / offline sim
-- --------------------------------------------------------------------------

-- | Internal state: (fixed dt, elapsed time so far). Now built on IO as
-- the base monad so callers can 'liftIO' (e.g. to print) while still
-- getting fully deterministic dt values.
newtype SimClock a = SimClock { unSimClock :: StateT (Float, Float) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Float, Float))

instance MonadClock SimClock where
  tick :: SimClock Float
  tick = SimClock $ do
    (dt, t) <- get
    put (dt, t + dt)
    pure dt
    
  currentTime :: SimClock Float
  currentTime = SimClock $ gets snd

-- | Run a 'SimClock' computation with a fixed step size, returning the
-- result along with the total elapsed simulation time.
runSimClock :: Float -> SimClock a -> IO (a, Float)
runSimClock dt (SimClock m) = do
  (a, (_, t)) <- runStateT m (dt, 0)
  pure (a, t)

-- | Like 'runSimClock' but discards the elapsed time.
evalSimClock :: Float -> SimClock a -> IO a
evalSimClock dt m = fst <$> runSimClock dt m

-- --------------------------------------------------------------------------
-- RealClock: wall-clock time via System.Clock's monotonic clock
-- --------------------------------------------------------------------------

-- | Mutable environment: last tick timestamp (ns) and accumulated elapsed
-- time (s). Not exported -- constructed via 'newRealClock' and threaded
-- opaquely through 'runRealClock'. Timing depends on the implementor.
data RealClockEnv = RealClockEnv
  { rcLastTimeNs :: IORef Integer
  , rcElapsed    :: IORef Float
  }

newtype RealClock a = RealClock { unRealClock :: ReaderT RealClockEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RealClockEnv)

instance MonadClock RealClock where
  tick :: RealClock Float
  tick = RealClock $ do
    env <- ask
    now <- liftIO $ toNanoSecs <$> getTime Monotonic
    prev <- liftIO $ readIORef (rcLastTimeNs env)
    let dt = fromIntegral (now - prev) / 1e9
    liftIO $ writeIORef (rcLastTimeNs env) now
    liftIO $ modifyIORef' (rcElapsed env) (+ dt)
    pure dt

  currentTime :: RealClock Float
  currentTime = RealClock $ do
    env <- ask
    liftIO $ readIORef (rcElapsed env)

-- | Create a fresh real-time clock environment, anchored to "now".
newRealClock :: IO RealClockEnv
newRealClock = do
  now     <- toNanoSecs <$> getTime Monotonic
  lastT   <- newIORef now
  elapsed <- newIORef 0
  pure $ RealClockEnv lastT elapsed

-- | Run a 'RealClock' computation against a given clock environment.
runRealClock :: RealClockEnv -> RealClock a -> IO a
runRealClock env (RealClock m) = runReaderT m env

-- | call the given clocking the function perodically forever 
clockR :: (forall m. (MonadClock m, MonadIO m) => m ()) -> Float -> IO ()
clockR cf dt = do
  env <- newRealClock
  runRealClock env $ forever $ do
    liftIO $ threadDelay $ round (dt * 1_000_000)
    cf

-- | simulate clocking the function, no delays, for n times
clockS :: (forall m. (MonadClock m, MonadIO m) => m ()) -> Float -> Int -> IO Float
clockS cf dt n = do
  (_, total) <- runSimClock dt (replicateM_ n cf)
  pure total
