{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubStatus (ClubStatus (..)) where

import BasicPrelude (readMay)
import Prelude

import Data.ByteString.Char8 qualified as BS
import Data.Csv (parseField)
import Data.Csv qualified as CSV
import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import TextShow (TextShow, fromString, showb, showt)
import Types.SqlParsing (parseTextField)

data ClubStatus = Active | Ineligible | Low | Suspended
  deriving (Eq, Generic, Ord, Read, Show)
instance CSV.FromField ClubStatus where
  parseField f = case readMay (T.strip $ T.pack $ BS.unpack f) of
    Just status -> pure status
    Nothing -> fail $ "Invalid ClubStatus: " <> BS.unpack f
instance FromField ClubStatus where
  fromField = parseTextField readMay "ClubStatus"
instance TextShow ClubStatus where
  showb = fromString . show
instance ToField ClubStatus where
  toField = SQLText . showt
