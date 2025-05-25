{-# LANGUAGE OverloadedRecordDot #-}

module PersistenceStore.SQLite.ClubPerformanceRow (projectEnhancedClubPerformanceReportToClubPerformanceRow) where

import Prelude

import Data.Text (Text, pack)
import Data.Time (showGregorian)
import Data.Time.Calendar.Month (Month(..))
import Database.SQLite.Simple.FromRow (FromRow(..))
import Database.SQLite.Simple.ToRow (ToRow(..))
import GHC.Generics (Generic)
import GHC.Num (integerToInt)
import TextShow (showt)

import Types.ClubNumber (ClubNumber(..))
import Types.ClubPerformanceReport qualified as CPR
import Types.ClubStatus (ClubStatus(..))

data ClubPerformanceRow = ClubPerformanceRow
  { as_of :: !Text
  , month :: !Int
  , club_number :: !ClubNumber
  , club_status :: !ClubStatus
  , membership_base :: !Int
  , active_members  :: !Int
  , goals_met   :: !Int
  , level_1s    :: !Int
  , level_2s    :: !Int
  , more_level_2s :: !Int
  , level_3s    :: !Int
  , level_4s_5s_or_dtms :: !Int
  , more_level_4s_5s_or_dtms :: !Int
  , new_members :: !Int
  , more_new_members :: !Int
  , summer_officers_trained :: !Int
  , winter_officers_trained :: !Int
  , dues_paid_october :: !Int
  , dues_paid_april :: !Int
  , officer_list_on_time :: !Int
  , distinguished_status :: !Text
  } deriving Generic
instance FromRow ClubPerformanceRow
instance ToRow ClubPerformanceRow

projectEnhancedClubPerformanceReportToClubPerformanceRow :: CPR.EnhancedClubPerformanceReport -> ClubPerformanceRow
projectEnhancedClubPerformanceReportToClubPerformanceRow ecpr@CPR.EnhancedClubPerformanceReport { CPR.month = CPR.MonthWrapper (MkMonth m) } = ClubPerformanceRow
  { as_of = pack $ showGregorian ecpr.asOf
  , month = integerToInt m
  , club_number = ecpr.cpr.clubNumber
  , club_status = ecpr.cpr.clubStatus
  , membership_base = ecpr.cpr.membershipBase
  , active_members  = ecpr.cpr.activeMembers
  , goals_met       = ecpr.cpr.goalsMet
  , level_1s        = ecpr.cpr.level1s
  , level_2s        = ecpr.cpr.level2s
  , more_level_2s   = ecpr.cpr.moreLevel2s
  , level_3s        = ecpr.cpr.level3s
  , level_4s_5s_or_dtms = ecpr.cpr.level4s5sOrDtms
  , more_level_4s_5s_or_dtms = ecpr.cpr.moreLevel4s5sOrDtms
  , new_members     = ecpr.cpr.newMembers
  , more_new_members = ecpr.cpr.moreNewMembers
  , summer_officers_trained = ecpr.cpr.summerOfficersTrained
  , winter_officers_trained = ecpr.cpr.winterOfficersTrained
  , dues_paid_october = ecpr.cpr.duesPaidOctober
  , dues_paid_april = ecpr.cpr.duesPaidApril
  , officer_list_on_time = ecpr.cpr.officerListOnTime
  , distinguished_status = showt ecpr.cpr.distinguishedStatus
  }
