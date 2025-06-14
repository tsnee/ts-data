{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite.Insert
Description : SQLite functions for saving data.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite.Insert
  ( saveClubIfNecessary
  , saveIntMeasurement
  , saveReport
  , saveTextMeasurement
  ) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple
  ( Connection
  , NamedParam (..)
  , executeNamed
  )
import Database.SQLite.Simple.ToField (ToField)
import UnliftIO (liftIO)
import Prelude

import AppM (AppM)
import PersistenceStore.Analyzer (analyze)
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import PersistenceStore.SQLite.Common
  ( TableName (..)
  , intMeasurementTable
  , textMeasurementTable
  , withDatabase
  )
import Types.ClubMetric (ClubMetric (ReportingMonth))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport
  ( ClubPerformanceRecord (..)
  , ClubPerformanceReport (..)
  , clubNumber
  )

saveReport :: ClubPerformanceReport -> AppM ()
saveReport report = withDatabase $ flip saveReportWithConnection report

saveReportWithConnection :: Connection -> ClubPerformanceReport -> AppM ()
saveReportWithConnection conn ClubPerformanceReport{dayOfRecord, month, records} = traverse_ (saveRecord conn dayOfRecord month) records

saveRecord :: Connection -> Day -> Month -> ClubPerformanceRecord -> AppM ()
saveRecord conn dayOfRecord month record = do
  let clubId = clubNumber record
      monthMetricId = fromEnum ReportingMonth
      monthValue = case month of MkMonth m -> fromInteger m
      date = DbDate dayOfRecord
      reportingMonthMeasurement = Measurement{clubId, metricId = monthMetricId, value = monthValue, date}
      intRows = analyze (clubNumber record) dayOfRecord record
      textRows = analyze (clubNumber record) dayOfRecord record
  saveClubIfNecessary conn clubId
  traverse_ (saveIntMeasurement conn) (reportingMonthMeasurement : intRows)
  traverse_ (saveTextMeasurement conn) textRows

saveClubIfNecessary :: Connection -> ClubNumber -> AppM ()
saveClubIfNecessary conn (ClubNumber clubNumber) =
  liftIO $
    executeNamed
      conn
      "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
      [":clubNumber" := clubNumber]

saveIntMeasurement :: Connection -> Measurement Int -> AppM ()
saveIntMeasurement = saveMeasurement intMeasurementTable

saveTextMeasurement :: Connection -> Measurement Text -> AppM ()
saveTextMeasurement = saveMeasurement textMeasurementTable

{- ORMOLU_DISABLE -}
saveMeasurement :: forall a. ToField a => TableName -> Connection -> Measurement a -> AppM ()
saveMeasurement (TableName tableName) conn Measurement{clubId, metricId, value, date} =
  liftIO $ executeNamed
    conn
    query
    [ ":clubId"   := clubId
    , ":metricId" := metricId
    , ":value"    := value
    , ":date"     := date
    ]
  where
    query = mconcat
      [ "INSERT OR FAIL INTO ", tableName, " (club_id, metric_id, value, date) "
      , "SELECT :clubId, :metricId, :value, :date "
      , "WHERE NOT EXISTS ("
      ,   "SELECT 1 FROM ", tableName
      ,   " WHERE club_id = :clubId "
      ,   "AND metric_id = :metricId AND value = :value "
      ,   "AND date = ("
      ,     "SELECT MAX(date) FROM ", tableName
      ,     " WHERE club_id = :clubId AND metric_id = :metricId AND date < :date"
      ,   ")"
      , ");"
      ]
{- ORMOLU_ENABLE -}
