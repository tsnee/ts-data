{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite.Common
Description : Persistence code specific to the SQLite implementation.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite.Common
  ( TableName (..)
  , intMeasurementTable
  , testDatabase
  , textMeasurementTable
  , withDatabase
  ) where

import Control.Monad.Reader (MonadIO, MonadReader, ask)
import Database.SQLite.Simple
  ( Connection
  , Query (..)
  , close
  , execute_
  , open
  )
import UnliftIO (MonadUnliftIO, bracket, liftIO)
import Prelude

import Types.AppEnv (AppEnv (..))
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

newtype TableName = TableName Query

testDatabase :: DatabaseName
testDatabase = DatabaseName ":memory:"

withDatabase
  :: forall a m. (MonadIO m, MonadReader AppEnv m, MonadUnliftIO m) => (Connection -> m a) -> m a
withDatabase = bracket openDatabase (liftIO . close)

openDatabase :: (MonadIO m, MonadReader AppEnv m) => m Connection
openDatabase = do
  AppEnv{conf = Conf{databaseName = DatabaseName dbName}} <- ask
  conn <- liftIO $ open dbName
  liftIO $ execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

intMeasurementTable :: TableName
intMeasurementTable = TableName "int_measurements"

textMeasurementTable :: TableName
textMeasurementTable = TableName "text_measurements"
