module Unit.Libs.Download.Time where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Download.Time (calculatePauseMicros)

secondsPerMinute :: Integer
secondsPerMinute = 60

millisPerSecond :: Integer
millisPerSecond = 1000

microsPerMilli :: Integer
microsPerMilli = 1000

microsPerSecond :: Integer
microsPerSecond = microsPerMilli * millisPerSecond

picosPerMicro :: Integer
picosPerMicro = 1000

picosPerSecond :: Integer
picosPerSecond = picosPerMicro * microsPerSecond

tests :: TestTree
tests =
  testGroup
    "Libs.Download.Time"
    [ testGroup
        "calculatePause"
        [ testCase "One instantaneous request per minute needs 60M micros" $ do
            let actual = calculatePauseMicros 1 0 0
                expected = pure $ fromIntegral $ 60 * microsPerSecond
            actual @?= expected
        , testCase "Four instantaneous requests per minute need 15M micros" $ do
            let actual = calculatePauseMicros 4 0 0
                expected = pure $ fromIntegral $ 15 * microsPerSecond
            actual @?= expected
        , testCase "Six 10s requests per minute need no pause" $ do
            let actual = calculatePauseMicros 6 0 $ 10 * picosPerSecond
                expected = Nothing
            actual @?= expected
        , testCase "Sixty 1s requests per minute need no pause" $ do
            let actual = calculatePauseMicros 60 0 $ 1 * picosPerSecond
                expected = Nothing
            actual @?= expected
        , testCase "Fifty 1s requests per minute need 200ms" $ do
            let actual = calculatePauseMicros 50 0 $ 1 * picosPerSecond
                expected = pure $ fromIntegral $ 200 * microsPerMilli
            actual @?= expected
        , testCase "Five 10s requests per minute need 2s" $ do
            let actual = calculatePauseMicros 5 0 $ 10 * picosPerSecond
                expected = pure $ fromIntegral $ 2 * microsPerSecond
            actual @?= expected
        , testCase "Thirty 2s requests per minute need no pause" $ do
            let actual = calculatePauseMicros 30 0 $ 2 * picosPerSecond
                expected = Nothing
            actual @?= expected
        , testCase "Three hundred 200ms requests per minute need no pause" $ do
            let actual = calculatePauseMicros 300 0 $ 200 * picosPerMicro * microsPerMilli
                expected = Nothing
            actual @?= expected
        , testCase "Two 25s requests per minute need 5M micros" $ do
            let actual = calculatePauseMicros 2 0 $ 25 * picosPerSecond
                expected = pure 5000000
            actual @?= expected
        ]
    ]
