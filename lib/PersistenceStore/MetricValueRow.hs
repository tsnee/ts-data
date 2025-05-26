module PersistenceStore.MetricValueRow (DbDate (..), MetricValueRow (..)) where

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

data MetricValueRow a = MetricValueRow {clubId :: ClubNumber, metricId :: Int, value :: a, date :: DbDate}
  deriving (Generic)
instance FromRow (MetricValueRow Integer)
instance FromRow (MetricValueRow Text)
instance ToRow (MetricValueRow Integer)
instance ToRow (MetricValueRow Text)
