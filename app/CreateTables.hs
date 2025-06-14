{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (Query)
import Katip (Severity (..), Verbosity (..))
import Prelude

import AppM (runAppM)
import PersistenceStore.SQLite.Tables (createTables)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))
import Options (parseConf)

newtype ColumnType = ColumnType Query

main :: IO ()
main = do
  conf <-
    parseConf
      Conf
        { databaseName = DatabaseName "dcp.sqlite"
        , environment = "dev"
        , namespace = "create-tables"
        , severity = DebugS
        , verbosity = V3
        }
  runAppM conf () createTables
