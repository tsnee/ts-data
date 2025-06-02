{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (Query)
import Prelude

import MonadStack (runAppM)
import PersistenceStore.SQLite (DatabaseName (..))
import PersistenceStore.Tables (createTables)

newtype ColumnType = ColumnType Query

main :: IO ()
main = runAppM "dev" "create-tables" () $ createTables (DatabaseName "dcp.sqlite")
