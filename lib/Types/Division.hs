{-# LANGUAGE OverloadedStrings #-}

module Types.Division (Division (..)) where

import Data.Csv qualified as CSV
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Safe (headMay)
import TextShow (Builder, TextShow, fromString, showb)
import Prelude

import PersistenceStore.FieldParsers (parseTextField)

data Division = DivisionNotAssigned | Division Char
  deriving Eq
instance CSV.FromField Division where
  parseField s
    | s == "0D" = pure DivisionNotAssigned
    | otherwise = Division <$> (CSV.parseField s :: CSV.Parser Char)
instance FromField Division where
  fromField = parseTextField parseDivision
   where
    parseDivision :: Text -> Maybe Division
    parseDivision "0" = pure DivisionNotAssigned
    parseDivision d = Division <$> (headMay . T.unpack) d
instance Show Division where
  show DivisionNotAssigned = "DivisionNotAssigned"
  show (Division d) = [d]
instance TextShow Division where
  showb DivisionNotAssigned = "DivisionNotAssigned" :: Builder
  showb (Division d) = fromString [d]
instance ToField Division where
  toField DivisionNotAssigned = SQLText "0"
  toField (Division d) = SQLText $ T.pack [d]
