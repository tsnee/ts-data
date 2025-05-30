{-# LANGUAGE DerivingVia #-}

module Types.District (District (..)) where

import Prelude

import Data.Csv qualified as CSV
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (FromStringShow (..), TextShow)

newtype District = District Int
  deriving (CSV.FromField, FromField, Show, ToField)
  deriving (TextShow) via FromStringShow District
