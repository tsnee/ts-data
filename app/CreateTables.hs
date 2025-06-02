{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (ReaderT (..), ask)
import Data.Foldable (traverse_)
import Database.SQLite.Simple (Connection, NamedParam (..), Query, executeNamed, execute_)
import Katip (Severity (..), logFM, ls)
import TextShow (showt)
import UnliftIO (liftIO)
import Prelude

import MonadStack (AppM, runAppM)
import PersistenceStore.ClubMetrics (ClubMetrics)
import PersistenceStore.SQLite
  ( TableName (..)
  , intMeasurementTable
  , textMeasurementTable
  , withDatabase
  )

newtype ColumnType = ColumnType Query

main :: IO ()
main = runAppM "dev" "create-tables" () $ withDatabase $ do
  conn <- ask
  createClubTable conn
  createMetricNameTable conn
  createMeasurementTable conn intMeasurementTable (ColumnType "INTEGER")
  createMeasurementTable conn textMeasurementTable (ColumnType "TEXT")

createClubTable :: Connection -> ReaderT Connection AppM ()
createClubTable conn = do
  liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS clubs (id INTEGER NOT NULL PRIMARY KEY);"
  logFM InfoS "(Conditionally) created table clubs."

createMetricNameTable :: Connection -> ReaderT Connection AppM ()
createMetricNameTable conn = do
  liftIO $
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS metric_names \
      \( id   INTEGER NOT NULL PRIMARY KEY \
      \, name TEXT    NOT NULL \
      \);"
  logFM InfoS "(Conditionally) created table metric_names."
  let insert :: ClubMetrics -> ReaderT Connection AppM ()
      insert m =
        liftIO $
          executeNamed
            conn
            "INSERT INTO metric_names(id, name) VALUES (:id, :name) ON CONFLICT DO NOTHING"
            [":id" := fromEnum m, ":name" := showt m]
  traverse_ insert $ enumFrom minBound
  logFM InfoS "(Conditionally) populated table metric_names."

{- ORMOLU_DISABLE -}
createMeasurementTable :: Connection -> TableName -> ColumnType -> ReaderT Connection AppM ()
createMeasurementTable conn (TableName tableName) (ColumnType valueType) = do
  liftIO $
    execute_ conn $
      "CREATE TABLE IF NOT EXISTS "
        <> tableName
        <> "( club_id   INTEGER NOT NULL REFERENCES clubs(id) \
           \, date      DATE    NOT NULL \
           \, metric_id INTEGER NOT NULL REFERENCES metric_names(id) \
           \, value " <> valueType <> " NOT NULL \
           \, PRIMARY KEY (club_id, metric_id, date) \
           \);"
  logFM InfoS $ "(Conditionally) created table " <> ls (show tableName) <> "."
{- ORMOLU_ENABLE -}
