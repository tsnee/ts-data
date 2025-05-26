{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Data.Foldable (traverse_)
import Database.SQLite.Simple (Connection, NamedParam (..), executeNamed, execute_, open)
import TextShow (showt)

import PersistenceStore.Metrics (Metrics)
import PersistenceStore.SQLite (databaseName)

main :: IO ()
main = do
  conn <- open databaseName
  execute_ conn "PRAGMA foreign_keys = ON"
  createClubTable conn
  createMetricNameTable conn
  createIntMetricValueTable conn
  createTextMetricValueTable conn
  createEnumTable conn

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
  let insert :: Metrics -> IO ()
      insert m =
        executeNamed
          conn
          "INSERT INTO metric_names(id, name) VALUES (:id, :name) ON CONFLICT DO NOTHING"
          [":id" := fromEnum m, ":name" := showt m]
  traverse_ insert $ enumFrom minBound

createIntMetricValueTable :: Connection -> IO ()
createIntMetricValueTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS int_metric_values \
    \( club_id   INTEGER NOT NULL REFERENCES clubs(id) \
    \, metric_id INTEGER NOT NULL REFERENCES metric_names(id) \
    \, value     INTEGER NOT NULL \
    \, date      DATE    NOT NULL \
    \, PRIMARY KEY (club_id, metric_id, date) \
    \);"

createTextMetricValueTable :: Connection -> IO ()
createTextMetricValueTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS text_metric_values \
    \( club_id   INTEGER NOT NULL REFERENCES clubs(id) \
    \, metric_id INTEGER NOT NULL REFERENCES metric_names(id) \
    \, value     TEXT    NOT NULL \
    \, date      DATE    NOT NULL \
    \, PRIMARY KEY (club_id, metric_id, date) \
    \);"

createEnumTable :: Connection -> IO ()
createEnumTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS enums \
    \( name         TEXT    NOT NULL \
    \, metric_id    INTEGER NOT NULL REFERENCES metric_names(id) \
    \, metric_value INTEGER NOT NULL \
    \, PRIMARY KEY (metric_id, metric_value) \
    \);"
