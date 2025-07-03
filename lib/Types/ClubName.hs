module Types.ClubName (ClubName (..)) where

import Autodocodec (HasCodec)
import Data.Csv qualified as CSV (FromField)
import Data.OpenApi (ToParamSchema)
import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Servant.API (FromHttpApiData)

newtype ClubName = ClubName Text
  deriving
    (CSV.FromField, Eq, FromField, FromHttpApiData, HasCodec, Show, ToField, ToParamSchema)
