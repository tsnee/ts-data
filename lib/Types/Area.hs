{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Area (Area(..)) where

import           Prelude

import qualified Data.Csv                         as CSV
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.ToField   (ToField (..))

newtype Area = Area Int
  deriving (CSV.FromField, FromField, ToField, Read, Show)
