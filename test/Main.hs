{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Time (pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Servant.API (toUrlPiece)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import Download (parseFooter)
import PersistenceStore.SQLite (TableName (..), buildLoadMeasurementsQuery)
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReportSpec (ClubPerformanceReportSpec (..))
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "ts-data"
    [ testGroup
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
    , testGroup
        "PersistenceStore.SQLite"
        [ testGroup
            "buildLoadMeasurementsQuery"
            [ testCase "can build a query with empty [ClubMetrics]" $
                fst (buildLoadMeasurementsQuery (TableName "test") (ClubNumber 1) [] Nothing Nothing)
                  @?= "SELECT club_id, metric_id, value, date FROM test WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) ORDER BY date ASC;"
            , testCase "can build a query with singleton [ClubMetrics]" $
                fst (buildLoadMeasurementsQuery (TableName "test") (ClubNumber 1) [toEnum 3] Nothing Nothing)
                  @?= "SELECT club_id, metric_id, value, date FROM test WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) AND metric_id IN (3) ORDER BY date ASC;"
            , testCase "can build a query with multiple [ClubMetrics]" $
                fst
                  (buildLoadMeasurementsQuery (TableName "test") (ClubNumber 1) [toEnum 5, toEnum 7] Nothing Nothing)
                  @?= "SELECT club_id, metric_id, value, date FROM test WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) AND metric_id IN (5,7) ORDER BY date ASC;"
            ]
        ]
    , testGroup
        "Types.ClubPerformanceReportSpec"
        [ testGroup
            "toUrlPiece"
            [ testCase "Day of record is part of URL when present" $ do
                let actual =
                      toUrlPiece $
                        ClubPerformanceReportSpec
                          CSV
                          (District 117)
                          (YearMonth 2025 5)
                          (YearMonthDay 2025 5 15)
                          (ProgramYear 2024)
                    expected = "clubperformance~117~05/31/2025~05/15/2025~2024-2025"
                actual @?= expected
            ]
        ]
    ]
