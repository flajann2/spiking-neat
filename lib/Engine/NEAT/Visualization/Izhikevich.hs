
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.NEAT.Visualization.Izhikevich where

import Raylib.Core
  ( clearBackground
  , isKeyPressed
  , isMouseButtonDown
  , getMouseDelta
  , getMouseWheelMove
  )
import Raylib.Core.Models (drawGrid, drawLine3D, drawSphere)
import Raylib.Core.Text (drawText, drawFPS)
import Raylib.Types
  ( pattern Vector2
  , Vector3
  , pattern Vector3
  , pattern Rectangle
  , Color (..)
  , Camera3D (..)
  , CameraProjection (CameraPerspective)
  , KeyboardKey (KeyR)
  , MouseButton (MouseButtonRight)
  )
import Raylib.Util (whileWindowOpen0, withWindow, drawing, mode3D)
import Raylib.Util.Colors (white, red, green, blue)
import Raylib.Util.GUI (guiSlider, guiButton, guiLabel)
import Control.Monad (when)
import Data.IORef

data Vec3 = Vec3 !Float !Float !Float deriving (Show, Eq)

vec3 :: Float -> Float -> Float -> Vec3
vec3 = Vec3

toVector3 :: Vec3 -> Vector3
toVector3 (Vec3 x y z) = Vector3 x y z

-- | Izhikevich neuron state: membrane potential and recovery variable.
-- `v` and `u` are the two state variables of the model — everything else (a, b, c, d) just shapes how they interact.
-- 
-- **v — membrane potential**
-- This is the actual voltage across the neuron's membrane, in millivolts. It's the variable that spikes. The equation
-- 
-- ```
-- v' = 0.04v² + 5v + 140 - u + I
-- ```
-- 
-- is a fitted quadratic (Izhikevich reverse-engineered the coefficients 0.04, 5, 140 to match real cortical neuron dynamics using far less computation than Hodgkin-Huxley). The quadratic term is what gives you the fast upstroke of a spike — once `v` crosses a certain point the `0.04v²` term dominates and `v` shoots up toward +30 mV essentially in finite time, which is why the model needs the explicit `if v ≥ 30` reset rather than a smooth repolarization phase like Hodgkin-Huxley has. `I` is your injected/synaptic input current — the thing you'd drive with external stimulation.
-- 
-- **u — membrane recovery variable**
-- This isn't a real physical quantity in the way `v` is — it's a lumped abstraction standing in for the slow ionic currents that oppose depolarization, mainly K⁺ activation and Na⁺ inactivation. Its job is negative feedback: it grows in response to spiking (via the `+d` reset) and subtracts from `v`'s growth (the `-u` term), which is what eventually pulls the neuron back down and creates a refractory-like period.
-- 
-- The `a(bv - u)` dynamics mean `u` chases `bv` at rate `a` — so `u` is always lagging behind a scaled version of `v`, which is exactly the "slow feedback trailing a fast variable" structure that makes delay embedding work.
-- 
-- **Why this matters for your delay embedding**
-- 
-- Your choice of `(v(t), u(t), v(t-τ))` is interesting because `v` is fast/spiky and `u` is slow/smooth — they already form a natural fast-slow pair, similar to how the Lorenz system has a fast variable and slower ones. Using `v(t-τ)` as a stand-in for a third independent dimension works because `v` alone doesn't have enough memory in it to unfold the attractor (the spike reset destroys information), but `u` partially compensates since it integrates spike history. You'll likely see the reset rule show up as a visible discontinuity or fold in the embedded trajectory — worth watching for when `v` hits 30 and jumps to `c`, since that's a non-smooth event that a pure delay-embedding of a smooth ODE wouldn't have.
data NeuronState = NeuronState { nV :: !Float
                               , nU :: !Float
                               }

data IzhikevichParams = IzhikevichParams { za -- ^ recovery time scale
                                         , zb -- ^ sensitivity of u to subth
                                         , zc -- ^ post-spike reset value of
                                         , zd -- ^ post-spike bump to u
                                           :: !Float
                                         }

data CameraState = CameraState
  { orbitYaw      :: !Float
  , orbitPitch    :: !Float
  , orbitDistance :: !Float
  }
  
data Config = Config { windowWidth   :: Int
                     , windowHeight  :: Int
                     , neuronParams  :: IzhikevichParams
                     , initialCamera :: CameraState
                     , tau           :: Double  -- delay embedding lag
                     , initialI      :: Float   -- injected/synaptic current
                     , windowName    :: String
                     }

initialIzhikevichParams :: IzhikevichParams
initialIzhikevichParams = (IzhikevichParams 0.02 0.2 (-65.0) 8.0) 

defaultConfig :: Config
defaultConfig = (Config 2560 1600
                        initialIzhikevichParams
                        (CameraState 0.8 0.5 45.0)
                        0.0
                        10.0
                        "3D Phase Portrait - Izhikevich Neuron, Delay Embedding"
                )
defaultI = 10.0  -- injected/synaptic current

initialNeuronState :: IzhikevichParams -> NeuronState
initialNeuronState ns = NeuronState ns.zc (ns.zb * ns.zc)

-- | Delay-embedding window: how many 1ms steps back to sample v(t-tau) from.
delaySteps :: Int
delaySteps = 20

maxHistory :: Int
maxHistory = delaySteps + 1

-- | One 1ms integration step: two 0.5ms half-steps for v (stability near the
--   v^2 spike-threshold nonlinearity), one full step for u, then reset on spike.
-- The Izhikevich model boils a spiking neuron down to two coupled ODEs plus a reset rule:
-- 
-- ```
-- v' = 0.04v² + 5v + 140 - u + I
-- u' = a(bv - u)
-- if v ≥ 30: v ← c, u ← u + d
-- ```
-- 
-- Here's what each parameter does:
-- 
-- **a — recovery time scale**
-- Controls how fast `u` (the recovery variable) responds to changes in `v`. Small `a` (like 0.02) means slow recovery, so the neuron can sustain longer depolarizations or exhibit bursting. Larger `a` (like 0.1) means fast recovery, which tends to produce fast-spiking behavior.
-- 
-- **b — sensitivity of recovery to subthreshold voltage**
-- Controls how strongly `u` is coupled to `v` below threshold. Larger `b` makes `u` track `v` more tightly, which can produce low-threshold oscillations or resonator behavior. If `b < a`, you tend to get regular spiking/integrator dynamics; if `b > a`, you get resonator dynamics (the neuron responds preferentially to a certain input frequency).
-- 
-- **c — reset voltage**
-- After a spike, `v` snaps back to `c` (typically around -65 mV). More negative `c` gives a bigger after-spike hyperpolarization — this is one of the knobs that shapes bursting vs. tonic firing.
-- 
-- **d — after-spike jump in recovery**
-- After a spike, `u` gets bumped up by `d`. This is the key bursting/adaptation control — larger `d` means the recovery variable accumulates more with each spike, suppressing subsequent spikes and producing chattering or bursting patterns. Small `d` gives more regular tonic spiking.
-- 
-- **The classic combos** (from Izhikevich's 2003 paper) are worth having on hand since they map cleanly onto neuron classes:
-- - Regular spiking (RS): a=0.02, b=0.2, c=-65, d=8
-- - Intrinsically bursting (IB): a=0.02, b=0.2, c=-55, d=4
-- - Chattering (CH): a=0.02, b=0.2, c=-50, d=2
-- - Fast spiking (FS): a=0.1, b=0.2, c=-65, d=2
-- - Low-threshold spiking (LTS): a=0.02, b=0.25, c=-65, d=2
-- 
-- For your phase portrait visualizer, sweeping `d` while
-- holding the rest fixed is a nice way to show the qualitative transition
-- from tonic to bursting in the `(v, u, v(t-τ))` embedding — the bursts
-- show up as the trajectory looping through a slower,
-- larger-amplitude excursion before returning to the fast spike manifold.

stepIzh :: IzhikevichParams -> Float -> NeuronState -> NeuronState
stepIzh (IzhikevichParams a b c d) iCur (NeuronState v u) =
  let v1 = v  + 0.5 * (0.04 * v  * v  + 5 * v  + 140 - u + iCur)
      v2 = v1 + 0.5 * (0.04 * v1 * v1 + 5 * v1 + 140 - u + iCur)
      u' = u  + a * (b * v2 - u)
  in if v2 >= 30
     then NeuronState c (u' + d)
     else NeuronState v2 u'

mouseSensitivity :: Float
mouseSensitivity = 0.005

zoomSensitivity :: Float
zoomSensitivity = 2.0

minDistance, maxDistance, maxPitch :: Float
minDistance = 5.0
maxDistance = 200.0
maxPitch    = 1.5

orbitToVector3 :: CameraState -> Vector3
orbitToVector3 (CameraState yaw pitch dist) =
  let cosPitch = cos pitch
      x = dist * cosPitch * sin yaw
      y = dist * sin pitch
      z = dist * cosPitch * cos yaw
  in Vector3 x y z

run :: Config -> IO ()
run cfg = withWindow cfg.windowWidth cfg.windowHeight cfg.windowName 60 $ \_ -> do
  orbitRef      <- newIORef (CameraState 0.8 0.5 45.0)
  neuronRef     <- newIORef (initialNeuronState initialIzhikevichParams)
  vHistoryRef   <- newIORef ([] :: [Float])
  trailRef      <- newIORef ([] :: [Vec3])
  aRef          <- newIORef cfg.neuronParams.za
  bRef          <- newIORef cfg.neuronParams.zb
  cRef          <- newIORef cfg.neuronParams.zc
  dRef          <- newIORef cfg.neuronParams.zd
  iRef          <- newIORef cfg.initialI
  let maxPoints = 15000 :: Int

  whileWindowOpen0 $ do
    a    <- readIORef aRef
    b    <- readIORef bRef
    c    <- readIORef cRef
    d    <- readIORef dRef
    iCur <- readIORef iRef

    neuron <- readIORef neuronRef
    let neuron' = stepIzh (IzhikevichParams a b c d) iCur neuron
    writeIORef neuronRef neuron'

    vHist <- readIORef vHistoryRef
    let vHist' = take maxHistory (nV neuron' : vHist)
    writeIORef vHistoryRef vHist'

    let vDelayed =
          if length vHist' > delaySteps
            then vHist' !! delaySteps
            else nV neuron'
        embedPoint = vec3 (nV neuron') (nU neuron') vDelayed

    trail <- readIORef trailRef
    writeIORef trailRef (take maxPoints (embedPoint : trail))

    resetPressed <- isKeyPressed KeyR
    when resetPressed $ do
      writeIORef neuronRef neuron 
      writeIORef trailRef []
      writeIORef vHistoryRef []
    dragging        <- isMouseButtonDown MouseButtonRight
    Vector2 mdx mdy <- getMouseDelta
    wheel           <- getMouseWheelMove

    modifyIORef' orbitRef $ \(CameraState yaw pitch dist) ->
      let (yaw', pitch') =
            if dragging
              then ( yaw   - mdx * mouseSensitivity
                   , clampPitch (pitch - mdy * mouseSensitivity)
                   )
              else (yaw, pitch)
          dist' = clampDistance (dist - wheel * zoomSensitivity)
      in CameraState yaw' pitch' dist'

    orbit <- readIORef orbitRef
    let camera =
          Camera3D
            (orbitToVector3 orbit)
            (Vector3 0 0 0)
            (Vector3 0 1 0)
            50.0
            CameraPerspective

    trailPoints <- readIORef trailRef

    drawing $ do
      clearBackground (Color 15 45 25 255)
      mode3D camera $ do
        drawGrid 50 1.0
        drawAxes 18
        drawTrail trailPoints
        drawSphere (toVector3 embedPoint) 0.6 (Color 255 70 70 255)
      drawText "Izhikevich Neuron -- Phase Portrait (v, u, v(t-tau))" 20 20 24 white
      drawText "Drag mouse to orbit * Scroll to zoom * R = reset" 20 50 18 (Color 200 200 220 255)
      drawFPS 20 80

      -- GUI overlay: a/b/c/d/I sliders + redraw button
      guiLabel (Rectangle 20 110 200 20) "Izhikevich Parameters"

      (_, a') <- guiSlider (Rectangle 100 135 200 20) (Just "a") (Just (showF a)) a 0.0 0.2
      writeIORef aRef a'

      (_, b') <- guiSlider (Rectangle 100 165 200 20) (Just "b") (Just (showF b)) b 0.0 0.3
      writeIORef bRef b'

      (_, c') <- guiSlider (Rectangle 100 195 200 20) (Just "c") (Just (showF c)) c (-80.0) (-30.0)
      writeIORef cRef c'

      (_, d') <- guiSlider (Rectangle 100 225 200 20) (Just "d") (Just (showF d)) d 0.0 10.0
      writeIORef dRef d'

      (_, i') <- guiSlider (Rectangle 100 255 200 20) (Just "I") (Just (showF iCur)) iCur (-10.0) 40.0
      writeIORef iRef i'

      redrawClicked <- guiButton (Rectangle 20 290 120 30) (Just "Redraw")
      when redrawClicked $ do
        writeIORef neuronRef (initialNeuronState initialIzhikevichParams)
        writeIORef trailRef []
        writeIORef vHistoryRef []
  where
    clampPitch :: Float -> Float
    clampPitch p = max (-maxPitch) (min maxPitch p)

    clampDistance :: Float -> Float
    clampDistance d = max minDistance (min maxDistance d)

    showF :: Float -> String
    showF x = show (fromIntegral (round (x * 100)) / 100 :: Float)

    drawAxes :: Float -> IO ()
    drawAxes s = do
      let o = Vector3 0 0 0
      drawLine3D o (Vector3 s 0 0) red
      drawLine3D o (Vector3 0 s 0) green
      drawLine3D o (Vector3 0 0 s) blue

    drawTrail :: [Vec3] -> IO ()
    drawTrail [] = return ()
    drawTrail [_] = return ()
    drawTrail (p1@(Vec3 _ _ z1) : p2@(Vec3 _ _ z2) : rest) = do
      let v1 = toVector3 p1
          v2 = toVector3 p2
          avgZ = (z1 + z2) / 2
          t    = max 0 (min 1 ((avgZ + 65) / 100))
          r = round (30 + 200 * t) :: Int
          g = round (120 + 100 * t) :: Int
          b = round (220 - 100 * t) :: Int
          col = Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
      drawLine3D v1 v2 col
      drawTrail (p2 : rest)
