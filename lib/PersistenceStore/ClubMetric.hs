module PersistenceStore.ClubMetric (DbDate (..), ClubMetric (..)) where

import Prelude

import Data.Text (Text)
import Data.Time (Day)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromRow (FromRow (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))
import GHC.Generics (Generic)

import Types.ClubNumber (ClubNumber)

newtype DbDate = DbDate Day
  deriving (FromField, Generic, ToField)

data ClubMetric a = ClubMetric {clubId :: ClubNumber, metricId :: Int, value :: a, date :: DbDate}
  deriving (Generic)
instance FromRow (ClubMetric Integer)
instance FromRow (ClubMetric Text)
instance ToRow (ClubMetric Integer)
instance ToRow (ClubMetric Text)
