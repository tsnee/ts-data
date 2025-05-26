{-# LANGUAGE OverloadedStrings #-}

module Types.DistinguishedStatus (DistinguishedStatus (..)) where

import Prelude

import Data.ByteString.Char8 qualified as BS
import Data.Csv (parseField)
import Data.Csv qualified as CSV
import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

import Types.SqlParsing (parseTextField)

data DistinguishedStatus = Smedly | Presidents | Select | Distinguished | NotYet
  deriving (Enum, Eq, Read, Show)
instance CSV.FromField DistinguishedStatus where
  parseField f = case parseDistinguishedStatus (T.strip $ T.pack $ BS.unpack f) of
    Just status -> pure status
    Nothing -> fail $ "Invalid DistinguishedStatus: " <> BS.unpack f
instance FromField DistinguishedStatus where
  fromField = parseTextField parseDistinguishedStatus "DistinguishedStatus"
instance ToField DistinguishedStatus where
  toField = SQLInteger . toEnum . fromEnum

parseDistinguishedStatus :: T.Text -> Maybe DistinguishedStatus
parseDistinguishedStatus s = case s of
  "Presidents" -> pure Presidents
  "Select" -> pure Select
  "Distinguished" -> pure Distinguished
  _ -> pure NotYet
