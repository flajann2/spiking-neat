module Izhikevich where

import Numeric.LinearAlgebra
import System.Random (newStdGen, randomRs)
import Control.Monad (foldM)

-- ================================================================
-- Network parameters
-- ================================================================
ne, ni, n :: Int
ne = 800
ni = 200
n  = ne + ni

-- Build parameter vectors from random seeds
buildParams :: Vector Double -> Vector Double
            -> (Vector Double, Vector Double,
                Vector Double, Vector Double)
buildParams re ri =
  let a = vjoin [ 0.02 `scale` ones ne
                , fromList $ zipWith (\x -> (0.02 +) . (0.08 *)) (toList ri) (toList ri)
                ]
      b = vjoin [ 0.2  `scale` ones ne
                , fromList $ map (\x -> 0.25 - 0.05 * x) (toList ri)
                ]
      c = vjoin [ fromList $ map (\x -> -65 + 15 * x^2) (toList re)
                , (-65) `scale` ones ni
                ]
      d = vjoin [ fromList $ map (\x -> 8 - 6 * x^2) (toList re)
                , 2 `scale` ones ni
                ]
  in (a, b, c, d)
  where ones k = vector (replicate k 1.0)

-- ================================================================
-- Simulation state
-- ================================================================
data NetState = NetState
  { vsVar   :: Vector Double   -- membrane potentials
  , uVar    :: Vector Double   -- recovery variables
  , firings :: [(Int, Int)]    -- (time, neuron index)
  }

-- ================================================================
-- Single timestep
-- ================================================================
stepNet :: Matrix Double      -- synaptic weight matrix S
        -> Vector Double      -- a
        -> Vector Double      -- b
        -> Vector Double      -- c
        -> Vector Double      -- d
        -> Vector Double      -- thalamic input I
        -> Int                -- current time t
        -> NetState
        -> NetState
stepNet s a b c d thalI t (NetState v u spks) =
  let -- Indices of fired neurons
      fired    = filter (\i -> v ! i >= 30) [0 .. n-1]

      -- Record spikes
      newSpks  = map (t,) fired

      -- Reset fired neurons
      v1 = accum v (\_ ci -> ci) (zip fired (map (c !) fired))
      u1 = accum u (\ui di -> ui + di) (zip fired (map (d !) fired))

      -- Synaptic input from fired neurons
      synI = if null fired
               then konst 0 n
               else foldl1 add [ flatten (s ?? (All, Pos (idxs [i])))
                                | i <- fired ]

      totalI = thalI + synI

      -- Two half-steps of 0.5ms for numerical stability
      dv v_ = 0.04 * v_ * v_ + 5 * v_ + 140 - u1 + totalI
      v2   = v1 + 0.5 `scale` dv v1
      v3   = v2 + 0.5 `scale` dv v2

      -- Recovery variable update
      u2   = u1 + a * (b * v3 - u1)

  in NetState v3 u2 (spks ++ newSpks)

-- ================================================================
-- Full simulation: 1000 ms
-- ================================================================
simulate :: IO [(Int, Int)]
simulate = do
  gen <- newStdGen
  let rs    = randomRs (0.0, 1.0) gen
      re    = vector $ take ne rs
      ri    = vector $ take ni (drop ne rs)
      randS = take (n * (ne + ni)) (drop (ne + ni) rs)

      (a, b, c, d) = buildParams re ri

      -- Synaptic matrix: excitatory +0.5, inhibitory -1.0
      sExc  = (n >< ne) $ map (0.5 *)  $ take (n * ne) randS
      sInh  = (n >< ni) $ map negate   $ take (n * ni) (drop (n * ne) randS)
      s     = fromBlocks [[sExc, sInh]]

      v0    = konst (-65) n
      u0    = b * v0

      initState = NetState v0 u0 []

  finalState <- foldM (\st t -> do
      genT <- newStdGen
      let rsT  = randomRs (0.0, 1.0) genT
          iExc = vector $ map (\x -> 5 * (x * 2 - 1)) $ take ne rsT
          iInh = vector $ map (\x -> 2 * (x * 2 - 1)) $ take ni (drop ne rsT)
          thalI = vjoin [iExc, iInh]
      return $ stepNet s a b c d thalI t st
    ) initState [1..1000]

  return $ firings finalState
