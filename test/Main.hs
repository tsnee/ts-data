{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
import Prelude

import System.Persistence qualified as SP (tests)
import Unit.Libs qualified as UL (tests)
import Unit.MealyMachine qualified as UM (tests)
import Unit.Types qualified as UT (tests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ testGroup "Unit tests" [UL.tests, UT.tests, UM.tests]
      , testGroup "System tests" [SP.tests]
      ]
