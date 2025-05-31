{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Database.SQLite.Simple (Connection, NamedParam (..), Query, executeNamed, execute_)
import TextShow (showt)
import Prelude

import PersistenceStore.ClubMetrics (ClubMetrics)
import PersistenceStore.SQLite
  ( TableName (..)
  , intMeasurementTable
  , openDatabase
  , textMeasurementTable
  )

newtype ColumnType = ColumnType Query

main :: IO ()
main = do
  conn <- openDatabase
  createClubTable conn
  createMetricNameTable conn
  createMeasurementTable conn intMeasurementTable (ColumnType "INTEGER")
  createMeasurementTable conn textMeasurementTable (ColumnType "TEXT")

createClubTable :: Connection -> IO ()
createClubTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS clubs (id INTEGER NOT NULL PRIMARY KEY);"

createMetricNameTable :: Connection -> IO ()
createMetricNameTable conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS metric_names \
    \( id   INTEGER NOT NULL PRIMARY KEY \
    \, name TEXT    NOT NULL \
    \);"
  let insert :: ClubMetrics -> IO ()
      insert m =
        executeNamed
          conn
          "INSERT INTO metric_names(id, name) VALUES (:id, :name) ON CONFLICT DO NOTHING"
          [":id" := fromEnum m, ":name" := showt m]
  traverse_ insert $ enumFrom minBound

createMeasurementTable :: Connection -> TableName -> ColumnType -> IO ()
createMeasurementTable conn (TableName tableName) (ColumnType valueType) =
  execute_ conn $
    "CREATE TABLE IF NOT EXISTS "
      <> tableName
      <> "( club_id   INTEGER NOT NULL REFERENCES clubs(id) \
         \, date      DATE    NOT NULL \
         \, metric_id INTEGER NOT NULL REFERENCES metric_names(id) \
         \, value    "
      <> valueType
      <> " NOT NULL \
         \, PRIMARY KEY (club_id, metric_id, date) \
         \);"
