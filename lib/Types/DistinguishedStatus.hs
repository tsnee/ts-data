{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.DistinguishedStatus (DistinguishedStatus (..)) where

import Data.ByteString.Char8 qualified as BS
import Data.Csv (parseField)
import Data.Csv qualified as CSV
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (FromStringShow (..), TextShow)
import Prelude

import Types.SqlParsing (parseEnumField)

data DistinguishedStatus = Smedley | Presidents | Select | Distinguished | NotYet
  deriving (Bounded, Enum, Eq, Read, Show)
  deriving TextShow via FromStringShow DistinguishedStatus
instance CSV.FromField DistinguishedStatus where
  parseField f = case f of
    "X" -> pure Smedley
    "P" -> pure Presidents
    "S" -> pure Select
    "D" -> pure Distinguished
    "" -> pure NotYet
    _ -> fail $ "Invalid DistinguishedStatus: " <> BS.unpack f
instance FromField DistinguishedStatus where
  fromField = parseEnumField
instance ToField DistinguishedStatus where
  toField = SQLInteger . fromIntegral . fromEnum
