{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReport (ClubPerformanceRecord (..), ClubPerformanceReport (..)) where

import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import GHC.Generics (Generic)
import TextShow (FromStringShow (..), TextShow, showt)
import Prelude

import PersistenceStore.Analyzer (Analyzer (..))
import PersistenceStore.ClubMetric qualified as M
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Types.Area (Area (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubStatus (ClubStatus (..))
import Types.DistinguishedStatus (DistinguishedStatus)
import Types.District (District (..))
import Types.Division (Division (..))

data ClubPerformanceReport = ClubPerformanceReport
  { dayOfRecord :: !Day
  , month :: !Month
  , records :: ![ClubPerformanceRecord]
  }
  deriving (Generic, Show)
  deriving TextShow via FromStringShow ClubPerformanceReport

data ClubPerformanceRecord = ClubPerformanceRecord
  { district :: !District
  , division :: !Division
  , area :: !Area
  , clubNumber :: !ClubNumber
  , clubName :: !Text
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
  deriving TextShow via FromStringShow ClubPerformanceRecord
instance FromNamedRecord ClubPerformanceRecord where
  parseNamedRecord r =
    ClubPerformanceRecord
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

instance Analyzer ClubPerformanceRecord Int where
  analyze clubId date rec =
    [ Measurement
        { clubId
        , metricId = fromEnum M.District
        , value = case district rec of District d -> d
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.Area
        , value = case area rec of
            AreaNotAssigned -> -1
            Area a -> a
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.ClubStatus
        , value = fromEnum (clubStatus rec)
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.MembershipBase
        , value = membershipBase rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.ActiveMembers
        , value = activeMembers rec
        , date = DbDate date
        }
    , Measurement{clubId, metricId = fromEnum M.GoalsMet, value = goalsMet rec, date = DbDate date}
    , Measurement{clubId, metricId = fromEnum M.LevelOnes, value = level1s rec, date = DbDate date}
    , Measurement
        { clubId
        , metricId = fromEnum M.LevelTwos
        , value = level2s rec + moreLevel2s rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.LevelThrees
        , value = level3s rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.HigherLevels
        , value = level4s5sOrDtms rec + moreLevel4s5sOrDtms rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.NewMembers
        , value = newMembers rec + moreNewMembers rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.OfficersTrainedRoundOne
        , value = winterOfficersTrained rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.OfficersTrainedRoundTwo
        , value = summerOfficersTrained rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.DuesOnTimeOctober
        , value = duesPaidOctober rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.DuesOnTimeApril
        , value = duesPaidApril rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.OfficersListOnTime
        , value = officerListOnTime rec
        , date = DbDate date
        }
    , Measurement
        { clubId
        , metricId = fromEnum M.DistinguishedStatus
        , value = fromEnum (distinguishedStatus rec)
        , date = DbDate date
        }
    ]
instance Analyzer ClubPerformanceRecord Text where
  analyze clubId date rec =
    [ Measurement
        { clubId
        , metricId = fromEnum M.Division
        , value = showt (division rec)
        , date = DbDate date
        }
    , Measurement{clubId, metricId = fromEnum M.ClubName, value = clubName rec, date = DbDate date}
    ]
