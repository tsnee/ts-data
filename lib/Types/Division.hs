{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Division (Division (..), fromText) where

import Autodocodec (HasCodec, bimapCodec, codec, (<?>))
import Data.Csv qualified as CSV (FromField, Parser, parseField)
import Data.Text (Text)
import Data.Text qualified as T (pack, uncons)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import TextShow (Builder, TextShow, fromString, showb)
import Prelude

import PersistenceStore.FieldParsers (parseTextField)

data Division = DivisionNotAssigned | Division Char
  deriving (Eq, Generic, Ord)
instance CSV.FromField Division where
  parseField s
    | s == "0D" = pure DivisionNotAssigned
    | otherwise = Division <$> (CSV.parseField s :: CSV.Parser Char)
instance FromField Division where
  fromField = parseTextField fromText
instance HasCodec Division where
  codec = bimapCodec dec enc codec <?> "Division letter (A-Z), or 'DivisionNotAssigned' if not assigned"
   where
    enc DivisionNotAssigned = "DivisionNotAssigned"
    enc (Division d) = [d]

    dec "DivisionNotAssigned" = pure DivisionNotAssigned
    dec [c] = pure $ Division c
    dec _ = Left "Expected 'DivisionNotAssigned' or a single character (A-Z) for Division"
instance Show Division where
  show DivisionNotAssigned = "DivisionNotAssigned"
  show (Division d) = [d]
instance TextShow Division where
  showb DivisionNotAssigned = "DivisionNotAssigned" :: Builder
  showb (Division d) = fromString [d]
instance ToField Division where
  toField DivisionNotAssigned = SQLText "0"
  toField (Division d) = SQLText $ T.pack [d]

fromText :: Text -> Maybe Division
fromText "0" = pure DivisionNotAssigned
fromText division = Division . fst <$> T.uncons division
