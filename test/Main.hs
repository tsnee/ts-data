{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
import Prelude

import System.Persistence qualified as SP (tests)
import Unit.Libs.Download.MealyMachine qualified as UM (tests)
import Unit.Libs.Download.Parsers qualified as UP (tests)
import Unit.Libs.Serve qualified as US (tests)
import Unit.Libs.Types.ClubPerformanceReportDescriptor qualified as UC (tests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ testGroup "Unit tests" [UM.tests, UP.tests, US.tests, UC.tests]
      , testGroup "System tests" [SP.tests]
      ]
