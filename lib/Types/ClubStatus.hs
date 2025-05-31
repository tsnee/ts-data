{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubStatus (ClubStatus (..)) where

import BasicPrelude (readMay)
import Data.ByteString.Char8 qualified as BS
import Data.Csv (parseField)
import Data.Csv qualified as CSV
import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import TextShow (FromStringShow (..), TextShow)
import Prelude

import Types.SqlParsing (parseTextField)

data ClubStatus = Active | Ineligible | Low | Suspended
  deriving (Enum, Eq, Ord, Read, Show)
  deriving TextShow via FromStringShow ClubStatus
instance CSV.FromField ClubStatus where
  parseField f = case readMay (T.strip $ T.pack $ BS.unpack f) of
    Just status -> pure status
    Nothing -> fail $ "Invalid ClubStatus: " <> BS.unpack f
instance FromField ClubStatus where
  fromField = parseTextField readMay
instance ToField ClubStatus where
  toField = SQLInteger . toEnum . fromEnum
