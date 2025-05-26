{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite (SQLite, databaseName) where

import Prelude

import Data.Foldable (traverse_)
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
import PersistenceStore.Class (Persistable, PersistenceType, save)
import PersistenceStore.MetricValueRow (DbDate (..), MetricValueRow (..))
import PersistenceStore.Metrics (Metrics (ReportingMonth))
import Types.ClubPerformanceReport (EnhancedClubPerformanceReport (..), clubNumber)

databaseName :: String
databaseName = "dcp.sqlite"

data SQLite
instance PersistenceType SQLite

saveRow :: ToField a => Connection -> Query -> MetricValueRow a -> IO ()
saveRow conn tableName (MetricValueRow {clubId, metricId, value, date}) =
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
      <>    "SELECT 1 FROM "
      <>    tableName
      <>    " WHERE club_id = :clubId "
      <>    "AND metric_id = :metricId "
      <>    "AND value = :value "
      <>    "AND date = ("
      <>      "SELECT MAX(date) FROM "
      <>      tableName
      <>      " WHERE club_id = :clubId AND metric_id = :metricId AND date < :date"
      <>    ")"
      <> ");"

instance Persistable SQLite EnhancedClubPerformanceReport IO () where
  save _ EnhancedClubPerformanceReport {cpr, asOf, month} = do
    conn <- open databaseName
    execute_ conn "PRAGMA foreign_keys = ON"
    executeNamed
      conn
      "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
      [":clubNumber" := clubNumber cpr]
    let clubId = clubNumber cpr
        metricId = fromEnum ReportingMonth
        value = case month of MkMonth m -> fromInteger m
        date = DbDate asOf
        reportingMonthMetric = MetricValueRow {clubId, metricId, value, date}
        (intRows, textRows) = analyze (clubNumber cpr) asOf cpr
    traverse_ (saveRow conn "int_metric_values") (reportingMonthMetric : intRows)
    traverse_ (saveRow conn "text_metric_values") textRows
