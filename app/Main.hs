{-# LANGUAGE OverloadedStrings #-}

module Main where

import Download (download)
import Types

main :: IO ()
main = do
  csv <- download (ClubPerformanceReport CSV (District 117) "4/30/2025" "5/1/2025" (ProgramYear 2024))
  print csv
