{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite.Class
Description : Persistence code specific to the SQLite implementation.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite.Class
  ( DatabaseName (..)
  , TableName (..)
  , intMeasurementTable
  , testDatabase
  , textMeasurementTable
  , withDatabase
  ) where

import Database.SQLite.Simple
  ( Connection
  , Query (..)
  , close
  , execute_
  , open
  )
import UnliftIO (bracket, liftIO)
import Prelude

import MonadStack (AppM)

newtype DatabaseName = DatabaseName String
newtype TableName = TableName Query

testDatabase :: DatabaseName
testDatabase = DatabaseName ":memory:"

withDatabase :: forall a. DatabaseName -> (Connection -> AppM a) -> AppM a
withDatabase databaseName = bracket (openDatabase databaseName) (liftIO . close)

openDatabase :: DatabaseName -> AppM Connection
openDatabase (DatabaseName databaseName) = do
  conn <- liftIO $ open databaseName
  liftIO $ execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

intMeasurementTable :: TableName
intMeasurementTable = TableName "int_measurements"

textMeasurementTable :: TableName
textMeasurementTable = TableName "text_measurements"
