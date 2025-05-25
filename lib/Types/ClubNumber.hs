module Types.ClubNumber (ClubNumber (..)) where

import Data.Csv qualified as CSV
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

newtype ClubNumber = ClubNumber Int
  deriving (CSV.FromField, FromField, Show, ToField)
