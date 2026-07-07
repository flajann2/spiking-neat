{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Raylib.Core
  ( disableCursor
  , clearBackground
  , isKeyPressed
  )
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid, drawLine3D, drawSphere)
import Raylib.Core.Text (drawText, drawFPS)
import Raylib.Types
  ( Vector3
  , pattern Vector3
  , Color (..)
  , Camera3D (..)
  , CameraProjection (CameraPerspective)
  , CameraMode (CameraModeFree)
  , KeyboardKey (KeyR)
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

main :: IO ()
main = withWindow 1280 800 "3D Phase Portrait - Lorenz Attractor" 60 $ \_ -> do
  disableCursor

  let initialCamera =
        Camera3D
          (Vector3 0 22 42)   -- position
          (Vector3 0 0 0)     -- target
          (Vector3 0 1 0)     -- up
          50.0                -- fovy
          CameraPerspective   -- projection

  cameraRef     <- newIORef initialCamera
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

    cam    <- readIORef cameraRef
    newCam <- updateCamera cam CameraModeFree
    writeIORef cameraRef newCam

    trailPoints <- readIORef trailRef

    drawing $ do
      clearBackground (Color 15 15 25 255)
      mode3D newCam $ do
        drawGrid 25 1.0
        drawAxes 18
        drawTrail trailPoints
        drawSphere (toVector3 newPos) 0.2 (Color 255 70 70 255)
      drawText "Lorenz Attractor -- 3D Phase Portrait" 20 20 24 white
      drawText "Mouse + WASD to explore * R = reset" 20 50 18 (Color 200 200 220 255)
      drawFPS 20 80
  where
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
