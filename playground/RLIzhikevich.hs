{-# LANGUAGE OverloadedStrings #-}

module Main where

import Engine.NEAT.Visualization.Izhikevich
main :: IO ()
main = do
  run defaultConfig
