{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types.Division (Division(..)) where

import           Prelude

import qualified Data.Csv                         as CSV
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData(..))
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField   (ToField(..))
import           Safe                             (headMay)
import           TextShow                         (TextShow, fromString, showb, showt)

import           Types.SqlParsing                 (parseTextField)

newtype Division = Division Char
  deriving (CSV.FromField, Show)
instance FromField Division where
  fromField = parseTextField parseDivision "Division" where
    parseDivision :: T.Text -> Maybe Division
    parseDivision d = Division <$> (headMay . T.unpack) d
instance TextShow Division where
  showb = fromString . show
instance ToField Division where
  toField = SQLText . showt
