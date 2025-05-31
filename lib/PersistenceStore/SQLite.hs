{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite
  ( TableName (..)
  , intMeasurementTable
  , loadIntMeasurements
  , loadTextMeasurements
  , openDatabase
  , saveReport
  , textMeasurementTable
  ) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple
  ( Connection
  , NamedParam (..)
  , Query (..)
  , executeNamed
  , execute_
  , open
  , queryNamed
  )
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.ToField (ToField)
import Prelude

import PersistenceStore.Analyzer (analyze)
import PersistenceStore.ClubMetrics (ClubMetrics (ReportingMonth))
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport
  ( ClubPerformanceRecord (..)
  , ClubPerformanceReport (..)
  , clubNumber
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

loadIntMeasurements :: ClubNumber -> Maybe Day -> Maybe Day -> IO [Measurement Integer]
loadIntMeasurements = loadMeasurements intMeasurementTable

loadTextMeasurements :: ClubNumber -> Maybe Day -> Maybe Day -> IO [Measurement Text]
loadTextMeasurements = loadMeasurements textMeasurementTable

loadMeasurements
  :: FromRow (Measurement a) => TableName -> ClubNumber -> Maybe Day -> Maybe Day -> IO [Measurement a]
loadMeasurements (TableName tableName) clubNumber startM endM = do
  let fromClause = maybe "" f startM
      f = Query . T.pack . formatTime defaultTimeLocale " AND date >= %F"
      toClause = maybe "" g endM
      g = Query . T.pack . formatTime defaultTimeLocale " AND date <= %F"
      q =
        "SELECT club_id, metric_id, value, date FROM "
          <> tableName
          <> " WHERE club_id = :clubId"
          <> fromClause
          <> toClause
          <> ";"
  conn <- openDatabase
  queryNamed conn q [":clubId" := clubNumber]

saveReport :: ClubPerformanceReport -> IO ()
saveReport ClubPerformanceReport{dayOfRecord, month, records} = do
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
      reportingMonthMeasurement = Measurement{clubId, metricId = monthMetricId, value = monthValue, date}
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
