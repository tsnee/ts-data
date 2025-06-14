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

import Control.Monad.Reader (MonadReader, ask)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple
  ( Connection
  , NamedParam (..)
  , close
  , executeNamed
  , execute_
  , open
  )
import Database.SQLite.Simple.ToField (ToField)
import UnliftIO (MonadIO, liftIO)
import Prelude

import PersistenceStore.Analyzer (analyze)
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import PersistenceStore.SQLite.Common
  ( TableName (..)
  , intMeasurementTable
  , textMeasurementTable
  )
import Types.AppEnv (AppEnv (..))
import Types.ClubMetric (ClubMetric (ReportingMonth))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport
  ( ClubPerformanceRecord (..)
  , ClubPerformanceReport (..)
  , clubNumber
  )
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

saveReport
  :: (MonadIO m, MonadReader AppEnv m)
  => ClubPerformanceReport
  -> m ()
saveReport report = do
  AppEnv{conf = Conf{databaseName = DatabaseName dbName}} <- ask
  conn <- liftIO $ open dbName
  liftIO $ execute_ conn "PRAGMA foreign_keys = ON"
  saveReportWithConnection conn report
  liftIO $ close conn

saveReportWithConnection
  :: MonadIO m
  => Connection
  -> ClubPerformanceReport
  -> m ()
saveReportWithConnection conn ClubPerformanceReport{dayOfRecord, month, records} = traverse_ (saveRecord conn dayOfRecord month) records

saveRecord
  :: MonadIO m
  => Connection
  -> Day
  -> Month
  -> ClubPerformanceRecord
  -> m ()
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

saveClubIfNecessary :: MonadIO m => Connection -> ClubNumber -> m ()
saveClubIfNecessary conn (ClubNumber clubNumber) =
  liftIO $
    executeNamed
      conn
      "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
      [":clubNumber" := clubNumber]

saveIntMeasurement :: MonadIO m => Connection -> Measurement Int -> m ()
saveIntMeasurement = saveMeasurement intMeasurementTable

saveTextMeasurement :: MonadIO m => Connection -> Measurement Text -> m ()
saveTextMeasurement = saveMeasurement textMeasurementTable

{- ORMOLU_DISABLE -}
saveMeasurement :: forall m a. (MonadIO m, ToField a) => TableName -> Connection -> Measurement a -> m ()
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
