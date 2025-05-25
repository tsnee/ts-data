{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.District (District (..)) where

import Prelude

import Data.Csv qualified as CSV
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (TextShow, showb)

newtype District = District Int
  deriving (CSV.FromField, FromField, Show, ToField)
instance TextShow District where
  showb (District i) = showb i
