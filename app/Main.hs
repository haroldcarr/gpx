{-# LANGUAGE LambdaCase #-}

module Main where

import Gpx
import System.Environment

main :: IO ()
main  = getArgs >>= \case
  [x] -> gpx x
  x   -> error ("wrong args: " ++ (show x))
