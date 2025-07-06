{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Libs.Serve where

import Data.List.NonEmpty (NonEmpty (..), singleton)
import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import PersistenceStore.DbDate (DbDate (..))
import PersistenceStore.Measurement (Measurement (..))
import Serve.ClubMeasurement (buildIntSeries, calculateDateRange, normalize)
import Types.ClubMeasurementResponse (Codomain (..), Series (..))
import Types.ClubMetric (ClubMetric (..))
import Types.ClubNumber (ClubNumber (..))
import Unit.Common.Sorting (sortByDate)

tests :: TestTree
tests =
  testGroup
    "Libs.Serve"
    [ testGroup
        "ClubMeasurement"
        [ testGroup
            "buildIntSeries"
            [ testCase "Three row result set" $ do
                let clubId = ClubNumber 2490993
                    rs =
                      [ Measurement
                          { clubId
                          , metricId = fromEnum ActiveMembers
                          , value = 10
                          , date = DbDate (YearMonthDay 2025 5 1)
                          }
                          :| [ Measurement
                                 { clubId
                                 , metricId = fromEnum ActiveMembers
                                 , value = 20
                                 , date = DbDate (YearMonthDay 2025 5 3)
                                 }
                             ]
                      , singleton
                          Measurement
                            { clubId
                            , metricId = fromEnum MembershipBase
                            , value = 15
                            , date = DbDate (YearMonthDay 2025 5 1)
                            }
                      ]
                    actual = sortByDate $ buildIntSeries rs
                    expected =
                      [ Series
                          { label = "ActiveMembers"
                          , domain = ["2025-05-01", "2025-05-03"]
                          , codomain = IntCodomain [10, 20]
                          }
                      , Series
                          { label = "MembershipBase"
                          , domain = ["2025-05-01"]
                          , codomain = IntCodomain [15]
                          }
                      ]
                actual @?= expected
            ]
        , testGroup
            "calculateDateRange"
            [ testCase "No supplied dates, no data" $ do
                let today = YearMonthDay 2025 5 1
                    expected = Nothing
                    actual = calculateDateRange Nothing Nothing today []
                actual @?= expected
            , testCase "Supplied start date, no data" $ do
                let startM = Just $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Nothing
                    actual = calculateDateRange startM Nothing today []
                actual @?= expected
            , testCase "Supplied end date, no data" $ do
                let endM = Just $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Nothing
                    actual = calculateDateRange Nothing endM today []
                actual @?= expected
            , testCase "Supplied start and end dates, no data" $ do
                let startM = Just $ YearMonthDay 2025 4 29
                    endM = Just $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Nothing
                    actual = calculateDateRange startM endM today []
                actual @?= expected
            , testCase "No supplied dates, one datum" $ do
                let dbDate = DbDate $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (dbDate, DbDate today)
                    actual = calculateDateRange Nothing Nothing today [dbDate]
                actual @?= expected
            , testCase "Supplied start date, one datum before" $ do
                let dbDate = DbDate $ YearMonthDay 2025 4 29
                    start = YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (DbDate start, DbDate today)
                    actual = calculateDateRange (Just start) Nothing today [dbDate]
                actual @?= expected
            , testCase "Supplied start date, one datum after" $ do
                let start = YearMonthDay 2025 4 29
                    dbDate = DbDate $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (dbDate, DbDate today)
                    actual = calculateDateRange (Just start) Nothing today [dbDate]
                actual @?= expected
            , testCase "Supplied end date, one datum before" $ do
                let dbDate = DbDate $ YearMonthDay 2025 4 29
                    end = YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (dbDate, DbDate end)
                    actual = calculateDateRange Nothing (Just end) today [dbDate]
                actual @?= expected
            , -- impossible: Supplied end date, one datum after
              testCase "Supplied start and end dates, one datum before" $ do
                let dbDate = DbDate $ YearMonthDay 2025 4 28
                    start = YearMonthDay 2025 4 29
                    end = YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (DbDate start, DbDate end)
                    actual = calculateDateRange (Just start) (Just end) today [dbDate]
                actual @?= expected
            , testCase "Supplied start and end dates, one datum between" $ do
                let start = YearMonthDay 2025 4 28
                    dbDate = DbDate $ YearMonthDay 2025 4 29
                    end = YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (dbDate, DbDate end)
                    actual = calculateDateRange (Just start) (Just end) today [dbDate]
                actual @?= expected
            , testCase "No supplied dates, two data" $ do
                let dbDate0 = DbDate $ YearMonthDay 2025 4 29
                    dbDate1 = DbDate $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (dbDate0, DbDate today)
                    actual = calculateDateRange Nothing Nothing today [dbDate0, dbDate1]
                actual @?= expected
            , testCase "Supplied start date before two data" $ do
                let start = YearMonthDay 2025 4 28
                    dbDate0 = DbDate $ YearMonthDay 2025 4 29
                    dbDate1 = DbDate $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (dbDate0, DbDate today)
                    actual = calculateDateRange (Just start) Nothing today [dbDate0, dbDate1]
                actual @?= expected
            , testCase "Supplied start date between two data" $ do
                let dbDate0 = DbDate $ YearMonthDay 2025 4 28
                    start = YearMonthDay 2025 4 29
                    dbDate1 = DbDate $ YearMonthDay 2025 4 30
                    today = YearMonthDay 2025 5 1
                    expected = Just (DbDate start, DbDate today)
                    actual = calculateDateRange (Just start) Nothing today [dbDate0, dbDate1]
                actual @?= expected
            ]
        , testGroup
            "normalize"
            [ testCase "No data" $ do
                let start = DbDate $ YearMonthDay 2025 4 30
                    end = DbDate $ YearMonthDay 2025 5 1
                    expected = [] :: [NonEmpty (Measurement Int)]
                    actual = normalize (Just (start, end)) []
                actual @?= expected
            , testCase "One datum before singleton range" $ do
                let dbDate = DbDate $ YearMonthDay 2025 4 30
                    start = DbDate $ YearMonthDay 2025 5 1
                    end = start
                    fixture = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate}
                    expected = [singleton fixture{date = end}]
                    actual = normalize (Just (start, end)) [singleton fixture]
                actual @?= expected
            , testCase "One datum before range" $ do
                let dbDate = DbDate $ YearMonthDay 2025 4 29
                    start = DbDate $ YearMonthDay 2025 4 30
                    end = DbDate $ YearMonthDay 2025 5 1
                    fixture = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate}
                    expected = [fixture{date = start} :| [fixture{date = end}]]
                    actual = normalize (Just (start, end)) [singleton fixture]
                actual @?= expected
            , testCase "One datum at beginning of range" $ do
                let start = DbDate $ YearMonthDay 2025 4 30
                    dbDate = start
                    end = DbDate $ YearMonthDay 2025 5 1
                    fixture = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate}
                    expected = [fixture :| [fixture{date = end}]]
                    actual = normalize (Just (start, end)) [singleton fixture]
                actual @?= expected
            , testCase "One datum within range" $ do
                let start = DbDate $ YearMonthDay 2025 4 29
                    dbDate = DbDate $ YearMonthDay 2025 4 30
                    end = DbDate $ YearMonthDay 2025 5 1
                    fixture = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate}
                    expected = [fixture :| [fixture{date = end}]]
                    actual = normalize (Just (start, end)) [singleton fixture]
                actual @?= expected
            , testCase "One datum at end of range" $ do
                let start = DbDate $ YearMonthDay 2025 4 29
                    end = DbDate $ YearMonthDay 2025 5 1
                    dbDate = end
                    fixture = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate}
                    expected = [singleton fixture]
                    actual = normalize (Just (start, end)) [singleton fixture]
                actual @?= expected
            , testCase "Two data matching range" $ do
                let start = DbDate $ YearMonthDay 2025 4 28
                    dbDate0 = start
                    end = DbDate $ YearMonthDay 2025 4 30
                    dbDate1 = end
                    fixture0 = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate0}
                    fixture1 = Measurement{clubId = ClubNumber 1, metricId = 1, value = 1 :: Int, date = dbDate1}
                    expected = [fixture0 :| [fixture1]]
                    actual = normalize (Just (start, end)) [fixture0 :| [fixture1]]
                actual @?= expected
            , testCase "Two data straddling beginning of range" $ do
                let dbDate0 = DbDate $ YearMonthDay 2025 4 27
                    start = DbDate $ YearMonthDay 2025 4 28
                    dbDate1 = DbDate $ YearMonthDay 2025 4 29
                    end = DbDate $ YearMonthDay 2025 4 30
                    fixture0 = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate0}
                    fixture1 = Measurement{clubId = ClubNumber 1, metricId = 1, value = 1 :: Int, date = dbDate1}
                    expected = [fixture0{date = start} :| [fixture1, fixture1{date = end}]]
                    actual = normalize (Just (start, end)) [fixture0 :| [fixture1]]
                actual @?= expected
            , testCase "Two data overlapping end of range" $ do
                let dbDate0 = DbDate $ YearMonthDay 2025 4 27
                    start = DbDate $ YearMonthDay 2025 4 28
                    end = DbDate $ YearMonthDay 2025 4 29
                    dbDate1 = end
                    fixture0 = Measurement{clubId = ClubNumber 1, metricId = 1, value = 0 :: Int, date = dbDate0}
                    fixture1 = Measurement{clubId = ClubNumber 1, metricId = 1, value = 1 :: Int, date = dbDate1}
                    expected = [fixture0{date = start} :| [fixture1]]
                    actual = normalize (Just (start, end)) [fixture0 :| [fixture1]]
                actual @?= expected
            ]
        ]
    ]
