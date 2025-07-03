{-# LANGUAGE DerivingVia #-}

module Types.ClubStatus (ClubStatus (..)) where

import Data.Bifunctor (first)
import Data.Csv (parseField)
import Data.Csv qualified as CSV (FromField)
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Text.Read qualified as TR (Read, readEither)
import Prelude

import PersistenceStore.FieldParsers (parseTextField)

data ClubStatus = Active | Ineligible | Low | Suspended
  deriving (Enum, Eq, Ord, Show, TR.Read)
instance CSV.FromField ClubStatus where
  parseField f = do
    text <- parseField f
    case TR.readEither text of
      Left err -> fail err
      Right clubStatus -> pure clubStatus
instance FromField ClubStatus where
  fromField = parseTextField fromText
instance ToField ClubStatus where
  toField = SQLInteger . toEnum . fromEnum

fromText :: Text -> Either Text ClubStatus
fromText t = first T.pack $ TR.readEither $ T.unpack t
