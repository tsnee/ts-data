{-# LANGUAGE DerivingVia #-}

module Types.District (District (..)) where

import Autodocodec (HasCodec)
import Data.Csv qualified as CSV (FromField)
import Data.OpenApi (ToParamSchema)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Servant.API (FromHttpApiData, ToHttpApiData)
import Prelude

newtype District = District Int
  deriving (CSV.FromField, Eq, FromField, Show, ToField)
  deriving (FromHttpApiData, HasCodec, ToHttpApiData, ToParamSchema) via Int
