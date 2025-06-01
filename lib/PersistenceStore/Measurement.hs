{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module PersistenceStore.Measurement (DbDate (..), Measurement (..)) where

import Data.Text (Text)
import Data.Text as T (pack)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Database.SQLite.Simple (FromRow (..), SQLData (..), ToRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import TextShow (TextShow)
import TextShow.Data.Time ()
import TextShow.Generic (FromGeneric (..))
import Prelude

import Types.ClubNumber (ClubNumber (..))

newtype DbDate = DbDate Day
  deriving stock (Eq, Generic)
  deriving (FromField, TextShow, ToField) via Day

data Measurement a = Measurement {clubId :: !ClubNumber, metricId :: !Int, value :: !a, date :: !DbDate}
  deriving Generic
deriving instance Eq a => Eq (Measurement a)
deriving via
  FromGeneric (Measurement a)
  instance
    TextShow a => TextShow (Measurement a)
instance FromRow (Measurement Int) where
  fromRow = Measurement <$> field <*> field <*> field <*> field
instance FromRow (Measurement Text) where
  fromRow = Measurement <$> field <*> field <*> field <*> field
instance ToRow (Measurement Int) where
  toRow = genericToRow $ SQLInteger . fromIntegral
instance ToRow (Measurement Text) where
  toRow = genericToRow SQLText
genericToRow :: (a -> SQLData) -> Measurement a -> [SQLData]
genericToRow f Measurement{clubId = ClubNumber clubNumber, metricId, value, date = DbDate day} =
  [ SQLInteger $ fromIntegral clubNumber
  , SQLInteger $ fromIntegral metricId
  , f value
  , SQLText $ T.pack $ formatTime defaultTimeLocale "%F" day
  ]
