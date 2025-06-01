{-# LANGUAGE DerivingVia #-}

module Types.District (District (..)) where

import Data.Csv qualified as CSV
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (TextShow)
import Prelude

newtype District = District Int
  deriving (CSV.FromField, FromField, Show, ToField)
  deriving TextShow via Int
