{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

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
  , Color (..)
  , Camera3D (..)
  , CameraProjection (CameraPerspective)
  , KeyboardKey (KeyR)
  , MouseButton (MouseButtonLeft)
  )
import Raylib.Util (whileWindowOpen0, withWindow, drawing, mode3D)
import Raylib.Util.Colors (white, red, green, blue)
import Control.Monad (when)
import Data.IORef

data Vec3 = Vec3 !Float !Float !Float deriving (Show, Eq)

vec3 :: Float -> Float -> Float -> Vec3
vec3 = Vec3

toVector3 :: Vec3 -> Vector3
toVector3 (Vec3 x y z) = Vector3 x y z

sigma, rho, beta, dt :: Float
sigma = 10.0
rho   = 28.0
beta  = 8.0 / 3.0
dt    = 0.005

stepLorenz :: Vec3 -> Vec3
stepLorenz (Vec3 x y z) =
  let dx = sigma * (y - x)
      dy = x * (rho - z) - y
      dz = x * y - beta * z
  in Vec3 (x + dx * dt) (y + dy * dt) (z + dz * dt)

-- Orbit-camera state: yaw/pitch angles (radians) and distance from origin.
data OrbitState = OrbitState
  { orbitYaw      :: !Float
  , orbitPitch    :: !Float
  , orbitDistance :: !Float
  }

mouseSensitivity :: Float
mouseSensitivity = 0.005

zoomSensitivity :: Float
zoomSensitivity = 2.0

minDistance, maxDistance, maxPitch :: Float
minDistance = 5.0
maxDistance = 150.0
maxPitch    = 1.5   -- just under pi/2, so we never flip over the poles

-- Turn (yaw, pitch, distance) into a camera position orbiting the origin.
orbitToVector3 :: OrbitState -> Vector3
orbitToVector3 (OrbitState yaw pitch dist) =
  let cosPitch = cos pitch
      x = dist * cosPitch * sin yaw
      y = dist * sin pitch
      z = dist * cosPitch * cos yaw
  in Vector3 x y z

main :: IO ()
main = withWindow 2560 1600 "3D Phase Portrait - Lorenz Attractor, Mouse control" 60 $ \_ -> do
  orbitRef      <- newIORef (OrbitState 0.8 0.5 45.0)
  currentPosRef <- newIORef (vec3 0.1 0 0)
  trailRef      <- newIORef ([] :: [Vec3])
  let maxPoints = 15000 :: Int

  whileWindowOpen0 $ do
    pos <- readIORef currentPosRef
    let newPos = stepLorenz pos
    writeIORef currentPosRef newPos

    trail <- readIORef trailRef
    writeIORef trailRef (take maxPoints (newPos : trail))

    resetPressed <- isKeyPressed KeyR
    when resetPressed $ do
      writeIORef currentPosRef (vec3 0.1 0 0)
      writeIORef trailRef []

    -- Mouse-drag orbit: only rotate while the left button is held.
    dragging        <- isMouseButtonDown MouseButtonLeft
    Vector2 mdx mdy <- getMouseDelta
    wheel           <- getMouseWheelMove

    modifyIORef' orbitRef $ \(OrbitState yaw pitch dist) ->
      let (yaw', pitch') =
            if dragging
              then ( yaw   - mdx * mouseSensitivity
                   , clampPitch (pitch - mdy * mouseSensitivity)
                   )
              else (yaw, pitch)
          dist' = clampDistance (dist - wheel * zoomSensitivity)
      in OrbitState yaw' pitch' dist'

    orbit <- readIORef orbitRef
    let camera =
          Camera3D
            (orbitToVector3 orbit)  -- position, computed from yaw/pitch/distance
            (Vector3 0 0 0)         -- target: always the origin
            (Vector3 0 1 0)         -- up
            50.0
            CameraPerspective

    trailPoints <- readIORef trailRef

    drawing $ do
      clearBackground (Color 15 45 25 255)
      mode3D camera $ do
        drawGrid 50 1.0
        drawAxes 18
        drawTrail trailPoints
        drawSphere (toVector3 newPos) 0.2 (Color 255 70 70 255)
      drawText "Lorenz Attractor -- 3D Phase Portrait" 20 20 24 white
      drawText "Drag mouse to orbit * Scroll to zoom * R = reset" 20 50 18 (Color 200 200 220 255)
      drawFPS 20 80
  where
    clampPitch :: Float -> Float
    clampPitch p = max (-maxPitch) (min maxPitch p)

    clampDistance :: Float -> Float
    clampDistance d = max minDistance (min maxDistance d)

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
          t    = max 0 (min 1 ((avgZ + 5) / 35))
          r = round (30 + 200 * t) :: Int
          g = round (120 + 100 * t) :: Int
          b = round (220 - 100 * t) :: Int
          col = Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
      drawLine3D v1 v2 col
      drawTrail (p2 : rest)
