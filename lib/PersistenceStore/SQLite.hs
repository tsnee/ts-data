{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite (TableName (..), intMeasurementTable, openDatabase, saveReport, textMeasurementTable) where

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

newtype TableName = TableName Query

databaseName :: String
databaseName = "dcp.sqlite"

intMeasurementTable :: TableName
intMeasurementTable = TableName "int_measurements"

textMeasurementTable :: TableName
textMeasurementTable = TableName "text_measurements"

openDatabase :: IO Connection
openDatabase = do
  conn <- open databaseName
  execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

saveReport :: ClubPerformanceReport -> IO ()
saveReport ClubPerformanceReport {dayOfRecord, month, records} = do
  conn <- openDatabase
  traverse_ (saveRecord conn dayOfRecord month) records

saveRecord :: Connection -> Day -> Month -> ClubPerformanceRecord -> IO ()
saveRecord conn dayOfRecord month record = do
  executeNamed
    conn
    "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
    [":clubNumber" := clubNumber record]
  let clubId = clubNumber record
      monthMetricId = fromEnum ReportingMonth
      monthValue :: Int = case month of MkMonth m -> fromInteger m
      date = DbDate dayOfRecord
      reportingMonthMeasurement = Measurement {clubId, metricId = monthMetricId, value = monthValue, date}
      intRows = analyze (clubNumber record) dayOfRecord record
      textRows = analyze (clubNumber record) dayOfRecord record
  traverse_ (saveIntMeasurement conn) (reportingMonthMeasurement : intRows)
  traverse_ (saveTextMeasurement conn) textRows

saveIntMeasurement :: Connection -> Measurement Int -> IO ()
saveIntMeasurement = saveMeasurement intMeasurementTable

saveTextMeasurement :: Connection -> Measurement Int -> IO ()
saveTextMeasurement = saveMeasurement textMeasurementTable

{- ORMOLU_DISABLE -}
saveMeasurement :: ToField a => TableName -> Connection -> Measurement a -> IO ()
saveMeasurement (TableName tableName) conn Measurement{clubId, metricId, value, date} =
  executeNamed
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
