{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module System.Persistence where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (singleton)
import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))
import UnliftIO (liftIO)
import Prelude

import PersistenceStore.DbDate (DbDate (..))
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Insert (saveClubIfNecessary, saveIntMeasurement)
import PersistenceStore.SQLite.Query (loadIntMeasurementsWithConnection)
import PersistenceStore.SQLite.Tables (createTablesWithConnection)
import System.AppTestCase (appConnTestCase)
import Types.ClubNumber (ClubNumber (..))

tests :: TestTree
tests =
  testGroup
    "PersistenceStore.SQLite"
    [ testGroup
        "save/load round trip"
        [ appConnTestCase "persistenceStore" "Everything saved can be loaded when date range is unspecified" $ \conn ->
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
                loadIntMeasurementsWithConnection conn (ClubNumber 1) (singleton (toEnum 1)) Nothing Nothing
              liftIO $ expected @?= actual
        , appConnTestCase "persistenceStore" "Everything saved can be loaded when date range is specified" $ \conn ->
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
                  (singleton (toEnum 1))
                  (Just (YearMonthDay 2024 12 31))
                  (Just (YearMonthDay 2025 2 1))
              liftIO $ expected @?= actual
        , appConnTestCase "persistenceStore" "Date ranges can be loaded" $ \conn ->
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
                  (singleton (toEnum 1))
                  (Just (YearMonthDay 2025 1 4))
                  (Just (YearMonthDay 2025 1 6))
              liftIO $ expected @?= actual
        , appConnTestCase "persistenceStore" "No start date specified" $ \conn ->
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
                  (singleton (toEnum 1))
                  Nothing
                  (Just (YearMonthDay 2025 1 6))
              liftIO $ expected @?= actual
        , appConnTestCase "persistenceStore" "No end date specified" $ \conn ->
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
                  (singleton (toEnum 1))
                  (Just (YearMonthDay 2025 1 4))
                  Nothing
              liftIO $ expected @?= actual
        ]
    ]
