{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite.Class
Description : Persistence code specific to the SQLite implementation.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite.Class
  ( TableName (..)
  , intMeasurementTable
  , testDatabase
  , textMeasurementTable
  , withDatabase
  ) where

import Control.Monad.Reader (ask)
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
import Types.AppEnv (AppEnv (..))
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

newtype TableName = TableName Query

testDatabase :: DatabaseName
testDatabase = DatabaseName ":memory:"

withDatabase :: forall a. (Connection -> AppM a) -> AppM a
withDatabase = bracket openDatabase (liftIO . close)

openDatabase :: AppM Connection
openDatabase = do
  AppEnv{conf = Conf{db = DatabaseName dbName}} <- ask
  conn <- liftIO $ open dbName
  liftIO $ execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

intMeasurementTable :: TableName
intMeasurementTable = TableName "int_measurements"

textMeasurementTable :: TableName
textMeasurementTable = TableName "text_measurements"
