module Unit.Libs.Download.Time where

import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, parseTimeM)
import Refined.Unsafe (unsafeRefine)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Download.Time (calculatePauseMicros)

seconds :: Int -> NominalDiffTime
seconds = realToFrac

milliseconds :: Int -> NominalDiffTime
milliseconds ms = seconds $ ms * 1000

microseconds :: Int -> NominalDiffTime
microseconds us = seconds $ us * 1000000

picoseconds :: Int -> NominalDiffTime
picoseconds ps = seconds $ ps * 1000000000000

baseTime :: UTCTime
baseTime =
  fromMaybe (error "bad time format") $
    parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2025-06-04 12:34:56"

elapsed :: a -> (a -> NominalDiffTime) -> UTCTime
elapsed x f = addUTCTime (f x) baseTime

tests :: TestTree
tests =
  testGroup
    "Libs.Download.Time"
    [ testGroup
        "calculatePause"
        [ testCase "One instantaneous request per minute needs 60M micros" $ do
            let actual = calculatePauseMicros (unsafeRefine 1) baseTime $ elapsed 0 picoseconds
                expected = pure 60000000
            actual @?= expected
        , testCase "Four instantaneous requests per minute need 15M micros" $ do
            let actual = calculatePauseMicros (unsafeRefine 4) baseTime $ elapsed 0 picoseconds
                expected = pure 15000000
            actual @?= expected
        , testCase "Six 10s requests per minute need no pause" $ do
            let actual = calculatePauseMicros (unsafeRefine 6) baseTime $ elapsed 10 seconds
                expected = Nothing
            actual @?= expected
        , testCase "Sixty 1s requests per minute need no pause" $ do
            let actual = calculatePauseMicros (unsafeRefine 60) baseTime $ elapsed 1 seconds
                expected = Nothing
            actual @?= expected
        , testCase "Fifty 1s requests per minute need 200ms" $ do
            let actual = calculatePauseMicros (unsafeRefine 50) baseTime $ elapsed 1 seconds
                expected = pure 200000
            actual @?= expected
        , testCase "Five 10s requests per minute need 2s" $ do
            let actual = calculatePauseMicros (unsafeRefine 5) baseTime $ elapsed 10 seconds
                expected = pure 2000000
            actual @?= expected
        , testCase "Thirty 2s requests per minute need no pause" $ do
            let actual = calculatePauseMicros (unsafeRefine 30) baseTime $ elapsed 2 seconds
                expected = Nothing
            actual @?= expected
        , testCase "Three hundred 200ms requests per minute need no pause" $ do
            let actual = calculatePauseMicros (unsafeRefine 300) baseTime $ elapsed 200 milliseconds
                expected = Nothing
            actual @?= expected
        , testCase "Two 25s requests per minute need 5M micros" $ do
            let actual = calculatePauseMicros (unsafeRefine 2) baseTime $ elapsed 25 seconds
                expected = pure 5000000
            actual @?= expected
        ]
    ]
