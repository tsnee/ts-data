{-# LANGUAGE OverloadedStrings #-}

module Types.Area (Area (..)) where

import Data.Csv qualified as CSV
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..), fromField)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Prelude

data Area = Area Int | AreaNotAssigned
  deriving (Read, Show)
instance CSV.FromField Area where
  parseField s
    | s == "0A" = pure AreaNotAssigned
    | otherwise = Area <$> (CSV.parseField s :: CSV.Parser Int)
instance FromField Area where
  fromField (Field (SQLText "0A") _) = pure AreaNotAssigned
  fromField f = Area <$> (fromField f :: Ok Int)
instance ToField Area where
  toField AreaNotAssigned = SQLInteger $ fromIntegral (-1 :: Int)
  toField (Area a) = SQLInteger $ fromIntegral a
