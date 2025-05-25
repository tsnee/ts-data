{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReport (ClubPerformanceReport (..), EnhancedClubPerformanceReport (..), MonthWrapper (..)) where

import Prelude

import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple (FromRow (..), ToRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..), fromField)
import Database.SQLite.Simple.ToField (ToField (..), toField)
import GHC.Generics (Generic)

import Types.Area (Area)
import Types.ClubNumber (ClubNumber)
import Types.ClubStatus (ClubStatus)
import Types.DistinguishedStatus (DistinguishedStatus)
import Types.District (District)
import Types.Division (Division)

data ClubPerformanceReport = ClubPerformanceReport
  { district :: !District
  , division :: !Division
  , area :: !Area
  , clubNumber :: !ClubNumber
  , clubName :: !T.Text
  , clubStatus :: !ClubStatus
  , membershipBase :: !Int
  , activeMembers :: !Int
  , goalsMet :: !Int
  , level1s :: !Int
  , level2s :: !Int
  , moreLevel2s :: !Int
  , level3s :: !Int
  , level4s5sOrDtms :: !Int
  , moreLevel4s5sOrDtms :: !Int
  , newMembers :: !Int
  , moreNewMembers :: !Int
  , summerOfficersTrained :: !Int
  , winterOfficersTrained :: !Int
  , duesPaidOctober :: !Int
  , duesPaidApril :: !Int
  , officerListOnTime :: !Int
  , distinguishedStatus :: !DistinguishedStatus
  }
  deriving (Generic, Show)
instance FromRow ClubPerformanceReport
instance ToRow ClubPerformanceReport
instance FromNamedRecord ClubPerformanceReport where
  parseNamedRecord r =
    ClubPerformanceReport
      <$> r .: "District"
      <*> r .: "Division"
      <*> r .: "Area"
      <*> r .: "Club Number"
      <*> r .: "Club Name"
      <*> r .: "Club Status"
      <*> r .: "Mem. Base"
      <*> r .: "Active Members"
      <*> r .: "Goals Met"
      <*> r .: "Level 1s"
      <*> r .: "Level 2s"
      <*> r .: "Add. Level 2s"
      <*> r .: "Level 3s"
      <*> r .: "Level 4s, Level 5s, or DTM award"
      <*> r .: "Add. Level 4s, Level 5s, or DTM award"
      <*> r .: "New Members"
      <*> r .: "Add. New Members"
      <*> r .: "Off. Trained Round 1"
      <*> r .: "Off. Trained Round 2"
      <*> r .: "Mem. dues on time Oct"
      <*> r .: "Mem. dues on time Apr"
      <*> r .: "Off. List On Time"
      <*> r .: "Club Distinguished Status"

newtype MonthWrapper = MonthWrapper Month
instance FromField MonthWrapper where
  fromField f = MonthWrapper . MkMonth <$> fromField f
instance ToField MonthWrapper where
  toField (MonthWrapper (MkMonth m)) = toField m

data EnhancedClubPerformanceReport = EnhancedClubPerformanceReport
  { cpr :: !ClubPerformanceReport
  , asOf :: !Day
  , month :: !MonthWrapper
  }
  deriving (Generic)
instance FromRow EnhancedClubPerformanceReport where
  fromRow = EnhancedClubPerformanceReport <$> fromRow <*> field <*> field
instance ToRow EnhancedClubPerformanceReport where
  toRow EnhancedClubPerformanceReport {cpr, asOf, month} = toRow cpr <> [toField asOf] <> [toField month]
