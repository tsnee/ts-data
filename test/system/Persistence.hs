{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module System.Persistence where

import Data.Foldable (traverse_)
import Data.Time (pattern YearMonthDay)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (liftIO)
import Prelude

import MonadStack (AppM)
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import PersistenceStore.SQLite.Insert (saveClubIfNecessary, saveIntMeasurement)
import PersistenceStore.SQLite.Query (loadIntMeasurementsWithConnection)
import PersistenceStore.SQLite.Tables (createTablesWithConnection)
import Types.ClubNumber (ClubNumber (..))
import System.AppTestCase (AppAssertion, appTestCase)


tests :: TestTree
tests =
  testGroup
    "PersistenceStore.SQLite"
    [ testGroup
        "save/load round trip"
        [ appTestCase "persistenceStore" "Everything saved can be loaded when date range is unspecified" $ \conn ->
            do
              createTablesWithConnection conn
              let testMeasurements =
                    [ Measurement
                        { clubId = ClubNumber 1
                        , metricId = 1
                        , value = idx
                        , date = DbDate (YearMonthDay 2025 1 idx)
                        }
                    | idx <- [1 .. 10]
                    ]
                  expected = testMeasurements
              saveClubIfNecessary conn $ ClubNumber 1
              traverse_ (saveIntMeasurement conn) testMeasurements
              actual <- loadIntMeasurementsWithConnection conn (ClubNumber 1) [toEnum 1] Nothing Nothing
              liftIO $ actual @?= expected
        , appTestCase "persistenceStore" "Everything saved can be loaded when date range is specified" $ \conn ->
            do
              createTablesWithConnection conn
              let testMeasurements =
                    [ Measurement
                        { clubId = ClubNumber 1
                        , metricId = 1
                        , value = idx
                        , date = DbDate (YearMonthDay 2025 1 idx)
                        }
                    | idx <- [1 .. 10]
                    ]
                  expected = testMeasurements
              saveClubIfNecessary conn $ ClubNumber 1
              traverse_ (saveIntMeasurement conn) testMeasurements
              actual <-
                loadIntMeasurementsWithConnection
                  conn
                  (ClubNumber 1)
                  [toEnum 1]
                  (Just (YearMonthDay 2024 12 31))
                  (Just (YearMonthDay 2025 2 1))
              liftIO $ actual @?= expected
        , appTestCase "persistenceStore" "Date ranges can be loaded" $ \conn ->
            do
              createTablesWithConnection conn
              let testMeasurements =
                    [ Measurement
                        { clubId = ClubNumber 1
                        , metricId = 1
                        , value = idx
                        , date = DbDate (YearMonthDay 2025 1 idx)
                        }
                    | idx <- [1 .. 10]
                    ]
                  expected = take 3 $ drop 3 testMeasurements
              saveClubIfNecessary conn $ ClubNumber 1
              traverse_ (saveIntMeasurement conn) testMeasurements
              actual <-
                loadIntMeasurementsWithConnection
                  conn
                  (ClubNumber 1)
                  [toEnum 1]
                  (Just (YearMonthDay 2025 1 4))
                  (Just (YearMonthDay 2025 1 6))
              liftIO $ actual @?= expected
        , appTestCase "persistenceStore" "No start date specified" $ \conn ->
            do
              createTablesWithConnection conn
              let testMeasurements =
                    [ Measurement
                        { clubId = ClubNumber 1
                        , metricId = 1
                        , value = idx
                        , date = DbDate (YearMonthDay 2025 1 idx)
                        }
                    | idx <- [1 .. 10]
                    ]
                  expected = take 6 testMeasurements
              saveClubIfNecessary conn $ ClubNumber 1
              traverse_ (saveIntMeasurement conn) testMeasurements
              actual <-
                loadIntMeasurementsWithConnection
                  conn
                  (ClubNumber 1)
                  [toEnum 1]
                  Nothing
                  (Just (YearMonthDay 2025 1 6))
              liftIO $ actual @?= expected
        , appTestCase "persistenceStore" "No end date specified" $ \conn ->
            do
              createTablesWithConnection conn
              let testMeasurements =
                    [ Measurement
                        { clubId = ClubNumber 1
                        , metricId = 1
                        , value = idx
                        , date = DbDate (YearMonthDay 2025 1 idx)
                        }
                    | idx <- [1 .. 10]
                    ]
                  expected = drop 3 testMeasurements
              saveClubIfNecessary conn $ ClubNumber 1
              traverse_ (saveIntMeasurement conn) testMeasurements
              actual <-
                loadIntMeasurementsWithConnection
                  conn
                  (ClubNumber 1)
                  [toEnum 1]
                  (Just (YearMonthDay 2025 1 4))
                  Nothing
              liftIO $ actual @?= expected
        ]
    ]
