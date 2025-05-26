{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReport (ClubPerformanceReport (..), EnhancedClubPerformanceReport (..)) where

import Prelude

import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple (FromRow (..), ToRow (..), field)
import Database.SQLite.Simple.ToField (ToField (..), toField)
import GHC.Generics (Generic)
import TextShow (TextShow, fromString, showb, showt)

import PersistenceStore.Analyzer (Analyzer (..))
import PersistenceStore.MetricValueRow (DbDate (..), MetricValueRow (..))
import PersistenceStore.Metrics qualified as M
import Types.Area (Area (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubStatus (ClubStatus (..))
import Types.DistinguishedStatus (DistinguishedStatus)
import Types.District (District (..))
import Types.Division (Division (..))

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

instance Analyzer ClubPerformanceReport where
  analyze clubId date cpr =
    (
      [ MetricValueRow
          { clubId
          , metricId = fromEnum M.District
          , value = case district cpr of District d -> d
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.Area
          , value = case area cpr of
              AreaNotAssigned -> -1
              Area a -> a
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.ClubStatus
          , value = fromEnum (clubStatus cpr)
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.MembershipBase
          , value = membershipBase cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.ActiveMembers
          , value = activeMembers cpr
          , date = DbDate date
          }
      , MetricValueRow {clubId, metricId = fromEnum M.GoalsMet, value = goalsMet cpr, date = DbDate date}
      , MetricValueRow {clubId, metricId = fromEnum M.LevelOnes, value = level1s cpr, date = DbDate date}
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.LevelTwos
          , value = (level2s cpr + moreLevel2s cpr)
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.LevelThrees
          , value = level3s cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.HigherLevels
          , value = (level4s5sOrDtms cpr + moreLevel4s5sOrDtms cpr)
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.NewMembers
          , value = (newMembers cpr + moreNewMembers cpr)
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.OfficersTrainedRoundOne
          , value = winterOfficersTrained cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.OfficersTrainedRoundTwo
          , value = summerOfficersTrained cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.DuesOnTimeOctober
          , value = duesPaidOctober cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.DuesOnTimeApril
          , value = duesPaidApril cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.OfficersListOnTime
          , value = officerListOnTime cpr
          , date = DbDate date
          }
      , MetricValueRow
          { clubId
          , metricId = fromEnum M.DistinguishedStatus
          , value = fromEnum (distinguishedStatus cpr)
          , date = DbDate date
          }
      ]
    ,
      [ MetricValueRow
          { clubId
          , metricId = fromEnum M.Division
          , value = showt (division cpr)
          , date = DbDate date
          }
      , MetricValueRow {clubId, metricId = fromEnum M.ClubName, value = clubName cpr, date = DbDate date}
      ]
    )

data EnhancedClubPerformanceReport = EnhancedClubPerformanceReport
  { cpr :: !ClubPerformanceReport
  , asOf :: !Day
  , month :: !Month
  }
  deriving (Generic, Show)
instance FromRow EnhancedClubPerformanceReport where
  fromRow = EnhancedClubPerformanceReport <$> fromRow <*> field <*> (MkMonth <$> field)
instance TextShow EnhancedClubPerformanceReport where
  showb = fromString . show
instance ToRow EnhancedClubPerformanceReport where
  toRow EnhancedClubPerformanceReport {cpr, asOf, month = MkMonth m} = toRow cpr <> [toField asOf] <> [toField m]
