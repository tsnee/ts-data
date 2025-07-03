module Types.ClubNumber (ClubNumber (..)) where

import Autodocodec (HasCodec)
import Data.Csv qualified as CSV (FromField)
import Data.OpenApi (ToParamSchema)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Servant.API (FromHttpApiData)

newtype ClubNumber = ClubNumber Int
  deriving
    (CSV.FromField, Eq, FromField, FromHttpApiData, HasCodec, Show, ToField, ToParamSchema)
