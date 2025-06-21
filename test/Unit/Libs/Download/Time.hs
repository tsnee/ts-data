module Unit.Libs.Download.Time where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Download.Time (calculatePauseMillis, picosPerMilli)

tests :: TestTree
tests =
  testGroup
    "Libs.Download.Time"
    [ testGroup
        "calculatePause"
        [ testCase "One instantaneous request per minute" $ do
            let actual = calculatePauseMillis 1 0 0
                expected = pure 60000
            actual @?= expected
        , testCase "Four instantaneous requests per minute" $ do
            let actual = calculatePauseMillis 4 0 0
                expected = pure 15000
            actual @?= expected
        , testCase "Sixty 100ms requests per minute" $ do
            let actual = calculatePauseMillis 60 0 $ 100 * picosPerMilli
                expected = pure 900
            actual @?= expected
        , testCase "Four 10s requests per minute" $ do
            let actual = calculatePauseMillis 4 0 $ 10000 * picosPerMilli
                expected = pure 5000
            actual @?= expected
        , testCase "Three hundred 200ms requests per minute" $ do
            let actual = calculatePauseMillis 300 0 $ 200 * picosPerMilli
                expected = Nothing
            actual @?= expected
        , testCase "Two 25s requests per minute" $ do
            let actual = calculatePauseMillis 2 0 $ 25000 * picosPerMilli
                expected = pure 5000
            actual @?= expected
        ]
    ]
