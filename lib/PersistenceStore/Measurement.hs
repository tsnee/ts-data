{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module PersistenceStore.Measurement (Measurement (..)) where

import Data.Text (Text)
import Data.Text as T (pack)
import Data.Time (defaultTimeLocale, formatTime)
import Database.SQLite.Simple (FromRow (..), SQLData (..), ToRow (..), field)
import GHC.Generics (Generic)
import Prelude

import PersistenceStore.DbDate (DbDate (..))
import Types.ClubNumber (ClubNumber (..))

data Measurement a = Measurement {clubId :: !ClubNumber, metricId :: !Int, value :: !a, date :: !DbDate}
  deriving Generic
deriving instance Eq a => Eq (Measurement a)
deriving instance Show a => Show (Measurement a)
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
