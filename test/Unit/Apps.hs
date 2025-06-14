{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Apps where

import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Download (parseFooter)

tests :: TestTree
tests =
  testGroup
    "Download"
    [ testGroup
        "parseFooter"
        [ testCase "Happy path" $ do
            let footer = "Month of Apr, As of 05/01/2025"
                actual = parseFooter footer
                expected = Right (4, YearMonthDay 2025 5 1)
            actual @?= expected
        , testCase "Bad month" $ do
            let footer = "Month of , As of 05/01/2025"
                actual = parseFooter footer
                expected = Left $ "Could not parse month from fragment 'Month of ' of CSV footer '" <> footer <> "'."
            actual @?= expected
        , testCase "Bad day" $ do
            let footer = "Month of Apr, As of 13/01/2025"
                actual = parseFooter footer
                expected =
                  Left $ "Could not parse date from fragment ', As of 13/01/2025' of CSV footer '" <> footer <> "'."
            actual @?= expected
        ]
    ]
