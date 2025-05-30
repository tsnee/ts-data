{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite (databaseName, saveReport) where

import Prelude

import Data.Foldable (traverse_)
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple (
  Connection,
  NamedParam (..),
  Query (..),
  executeNamed,
  execute_,
  open,
 )
import Database.SQLite.Simple.ToField (ToField)

import PersistenceStore.Analyzer (analyze)
import PersistenceStore.ClubMetrics (ClubMetrics (ReportingMonth))
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Types.ClubPerformanceReport (
  ClubPerformanceRecord (..),
  ClubPerformanceReport (..),
  clubNumber,
 )

databaseName :: String
databaseName = "dcp.sqlite"

saveMeasurement :: ToField a => Connection -> Query -> Measurement a -> IO ()
saveMeasurement conn tableName Measurement {clubId, metricId, value, date} =
  executeNamed
    conn
    query
    [":clubId" := clubId, ":metricId" := metricId, ":value" := value, ":date" := date]
 where
  query =
    "INSERT OR FAIL INTO "
      <> tableName
      <> " (club_id, metric_id, value, date) "
      <> "SELECT :clubId, :metricId, :value, :date "
      <> "WHERE NOT EXISTS ("
      <> "SELECT 1 FROM "
      <> tableName
      <> " WHERE club_id = :clubId "
      <> "AND metric_id = :metricId "
      <> "AND value = :value "
      <> "AND date = ("
      <> "SELECT MAX(date) FROM "
      <> tableName
      <> " WHERE club_id = :clubId AND metric_id = :metricId AND date < :date"
      <> ")"
      <> ");"

saveRecord :: Connection -> Day -> Month -> ClubPerformanceRecord -> IO ()
saveRecord conn dayOfRecord month record = do
  executeNamed
    conn
    "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
    [":clubNumber" := clubNumber record]
  let clubId = clubNumber record
      monthMetricId = fromEnum ReportingMonth
      monthValue = case month of MkMonth m -> fromInteger m
      date = DbDate dayOfRecord
      reportingMonthMeasurement = Measurement {clubId, metricId = monthMetricId, value = monthValue, date}
      (intRows, textRows) = analyze (clubNumber record) dayOfRecord record
  traverse_ (saveMeasurement conn "int_metric_values") (reportingMonthMeasurement : intRows)
  traverse_ (saveMeasurement conn "text_metric_values") textRows

saveReport :: ClubPerformanceReport -> IO ()
saveReport ClubPerformanceReport {dayOfRecord, month, records} = do
  conn <- open databaseName
  execute_ conn "PRAGMA foreign_keys = ON"
  traverse_ (saveRecord conn dayOfRecord month) records
