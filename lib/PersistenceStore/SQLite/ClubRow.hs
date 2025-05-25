{-# LANGUAGE OverloadedRecordDot #-}

module PersistenceStore.SQLite.ClubRow (projectClubPerformanceReportToClubRow) where

import Data.Text (Text)
import Database.SQLite.Simple.FromRow (FromRow(..))
import Database.SQLite.Simple.ToRow (ToRow(..))
import GHC.Generics (Generic)

import Types.Area (Area(..))
import Types.ClubNumber (ClubNumber(..))
import Types.ClubPerformanceReport qualified as CPR
import Types.District (District(..))
import Types.Division (Division(..))

data ClubRow = ClubRow {club_number :: ClubNumber, club_name :: Text, district :: District, division :: Division, area :: Area} deriving Generic
instance FromRow ClubRow
instance ToRow ClubRow

projectClubPerformanceReportToClubRow :: CPR.ClubPerformanceReport -> ClubRow
projectClubPerformanceReportToClubRow cpr = ClubRow
  { club_number = cpr.clubNumber
  , club_name = cpr.clubName
  , district = cpr.district
  , division = cpr.division
  , area = cpr.area
  }
