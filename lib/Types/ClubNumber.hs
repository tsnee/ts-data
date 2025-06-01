module Types.ClubNumber (ClubNumber (..)) where

import Autodocodec (HasCodec)
import Data.Csv qualified as CSV
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (TextShow)

newtype ClubNumber = ClubNumber Int
  deriving (CSV.FromField, Eq, FromField, HasCodec, Show, TextShow, ToField)
