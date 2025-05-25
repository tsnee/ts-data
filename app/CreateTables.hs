{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Database.SQLite.Simple (Connection, execute_, open)

import PersistenceStore.SQLite.Class (databaseName)

main :: IO ()
main = do
  conn <- open databaseName
  execute_ conn "PRAGMA foreign_keys = ON"
  createClubTable conn
  createClubAssignmentHistoryTable conn
  createClubNameHistoryTable conn
  createClubPerformanceTable conn

createClubTable :: Connection -> IO ()
createClubTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS club (club_number INTEGER NOT NULL PRIMARY KEY);"

createClubAssignmentHistoryTable :: Connection -> IO ()
createClubAssignmentHistoryTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS club_assignment_history ( \
    \club_number INTEGER NOT NULL REFERENCES club(club_number), \
    \district    INTEGER NOT NULL, \
    \division    CHAR    NOT NULL, \
    \area        INTEGER NOT NULL, \
    \start_date  TEXT    NOT NULL, \
    \end_date    TEXT \
    \);"

createClubNameHistoryTable :: Connection -> IO ()
createClubNameHistoryTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS club_name_history ( \
    \club_number INTEGER NOT NULL REFERENCES club(club_number), \
    \club_name   TEXT    NOT NULL, \
    \start_date  TEXT    NOT NULL, \
    \end_date    TEXT \
    \);"

createClubPerformanceTable :: Connection -> IO ()
createClubPerformanceTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS club_performance ( \
    \as_of       TEXT NOT NULL, \
    \month      INTEGER NOT NULL, \
    \club_number INTEGER NOT NULL REFERENCES club(club_number), \
    \club_status TEXT NOT NULL, \
    \membership_base INTEGER NOT NULL, \
    \active_members  INTEGER NOT NULL, \
    \goals_met   INTEGER NOT NULL, \
    \level_1s    INTEGER NOT NULL, \
    \level_2s    INTEGER NOT NULL, \
    \more_level_2s INTEGER NOT NULL, \
    \level_3s    INTEGER NOT NULL, \
    \level_4s_5s_or_dtms INTEGER NOT NULL, \
    \more_level_4s_5s_or_dtms INTEGER NOT NULL, \
    \new_members INTEGER NOT NULL, \
    \more_new_members INTEGER NOT NULL, \
    \summer_officers_trained INTEGER NOT NULL, \
    \winter_officers_trained INTEGER NOT NULL, \
    \dues_paid_october INTEGER NOT NULL, \
    \dues_paid_april INTEGER NOT NULL, \
    \officer_list_on_time INTEGER NOT NULL, \
    \distinguished_status TEXT NOT NULL, \
    \PRIMARY KEY (club_number, as_of) \
    \);"
