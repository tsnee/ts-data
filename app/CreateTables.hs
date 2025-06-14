{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (Query)
import Katip (Severity (..), Verbosity (..))
import Prelude

import MonadStack (runAppM)
import PersistenceStore.SQLite.Tables (createTables)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

newtype ColumnType = ColumnType Query

main :: IO ()
main =
  runAppM
    Conf
      { databaseName = DatabaseName "dcp.sqlite"
      , environment = "dev"
      , namespace = "create-tables"
      , severity = DebugS
      , verbosity = V3
      }
    ()
    createTables
