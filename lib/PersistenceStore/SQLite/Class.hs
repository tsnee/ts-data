{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite.Class (SQLite, databaseName) where

import Prelude

import Data.Time (Day)
import Database.SQLite.Simple (NamedParam(..), execute, execute_, open, queryNamed)
import Safe (headMay)

import PersistenceStore.Class (Persistable, PersistenceType, Retrievable, retrieve, save)
import PersistenceStore.SQLite.ClubPerformanceRow (projectEnhancedClubPerformanceReportToClubPerformanceRow)
import PersistenceStore.SQLite.ClubRow (projectClubPerformanceReportToClubRow)
import Types.ClubNumber (ClubNumber(..))
import Types.ClubPerformanceReport as CPR

databaseName :: String
databaseName = "dcp.sqlite"

data SQLite
instance PersistenceType SQLite

instance Persistable SQLite EnhancedClubPerformanceReport IO () where
  save _ ecpr = do
    conn <- open databaseName
    execute_ conn "PRAGMA foreign_keys = ON"
    let clubRow = projectClubPerformanceReportToClubRow ecpr.cpr
    execute conn "INSERT INTO club VALUES (?, ?, ?, ?, ?);" clubRow
    let clubPerformanceRow = projectEnhancedClubPerformanceReportToClubPerformanceRow ecpr
    execute conn "INSERT INTO club_performance VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" clubPerformanceRow

instance Retrievable SQLite (ClubNumber, Maybe Day) IO CPR.EnhancedClubPerformanceReport where
  retrieve _ (clubNumber, dayM) = do
    conn <- open databaseName
    execute_ conn "PRAGMA foreign_keys = ON"
    let clubQuery =
          "SELECT \
          \c.district, \
          \c.division, \
          \c.area, \
          \c.club_number AS clubNumber, \
          \c.club_name AS clubName, \
          \p.club_status AS clubStatus, \
          \p.membership_base AS membershipBase, \
          \p.active_members AS activeMembers, \
          \p.goals_met AS goalsMet, \
          \p.level_1s AS level1s, \
          \p.level_2s AS level2s, \
          \p.more_level_2s AS moreLevel2s, \
          \p.level_3s AS level3s, \
          \p.level_4s_5s_or_dtms AS level4s5sOrDtms, \
          \p.more_level_4s_5s_or_dtms AS moreLevel4s5sOrDtms, \
          \p.new_members AS newMembers, \
          \p.more_new_members AS moreNewMembers, \
          \p.summer_officers_trained AS summerOfficersTrained, \
          \p.winter_officers_trained AS winterOfficersTrained, \
          \p.dues_paid_october AS duesPaidOctober, \
          \p.dues_paid_april AS duesPaidApril, \
          \p.officer_list_on_time AS officerListOnTime, \
          \p.distinguished_status AS distinguishedStatus, \
          \p.as_of AS asOf, \
          \p.month \
          \FROM club c INNER JOIN club_performance p ON c.club_number \
          \WHERE c.club_number = :n"
    let completeQuery = foldl (\q _ -> q <> " AND as_of = :d") clubQuery dayM 
    rows <- queryNamed conn completeQuery [":c" := clubNumber, ":d" := dayM]
    pure $ headMay rows
