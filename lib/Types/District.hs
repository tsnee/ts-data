{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.District (District(..)) where

import           Prelude

import qualified Data.Csv                         as CSV
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.ToField   (ToField (..))
import           TextShow                         (TextShow, showb)

newtype District = District Int
  deriving (CSV.FromField, FromField, ToField, Show)
instance TextShow District where
  showb (District i) = showb i
