{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
import Prelude

import System.Persistence qualified as SP (tests)
import Unit.Apps qualified as UA (tests)
import Unit.Libs qualified as UL (tests)
import Unit.Types qualified as UT (tests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ testGroup "Unit tests" [UA.tests, UL.tests, UT.tests]
      , testGroup "System tests" [SP.tests]
      ]
