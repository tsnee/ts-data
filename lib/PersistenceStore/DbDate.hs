{-# LANGUAGE DerivingVia #-}

module PersistenceStore.DbDate (DbDate (..)) where

import Data.Time (Day)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromRow (FromRow (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)

newtype DbDate = DbDate {fromDbDate :: Day}
  deriving stock (Eq, Generic)
  deriving (FromField, Show, ToField) via Day

instance FromRow DbDate
