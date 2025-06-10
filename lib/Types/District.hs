{-# LANGUAGE DerivingVia #-}

module Types.District (District (..)) where

import Autodocodec (HasCodec)
import Data.Csv qualified as CSV
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (TextShow)
import Prelude

newtype District = District Int
  deriving (CSV.FromField, Eq, FromField, Show, ToField)
  deriving HasCodec via Int
  deriving TextShow via Int
