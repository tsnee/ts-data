{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (Query)
import Katip (Severity (DebugS))
import Prelude

import MonadStack (runAppM)
import PersistenceStore.SQLite.Class (DatabaseName (..))
import PersistenceStore.SQLite.Tables (createTables)

newtype ColumnType = ColumnType Query

main :: IO ()
main = runAppM "dev" "create-tables" () DebugS $ createTables (DatabaseName "dcp.sqlite")
