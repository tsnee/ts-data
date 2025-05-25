{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Database.SQLite.Simple (execute_, open)

import PersistenceStore.SQLite (databaseName)

main :: IO ()
main = do
  createClubTable
  createClubPerformanceTable

createClubTable :: IO ()
createClubTable = do
  conn <- open databaseName
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS club ( \
    \club_number INTEGER PRIMARY KEY, \
    \club_name   TEXT, \
    \district    INTEGER, \
    \division    CHAR, \
    \area        INTEGER \
    \);"

createClubPerformanceTable :: IO ()
createClubPerformanceTable = do
  conn <- open databaseName
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS club_performance ( \
    \as_of       TEXT, \
    \month      INTEGER, \
    \club_number INTEGER REFERENCES club(club_number), \
    \club_status TEXT, \
    \membership_base INTEGER, \
    \active_members  INTEGER, \
    \goals_met   INTEGER, \
    \level_1s    INTEGER, \
    \level_2s    INTEGER, \
    \more_level_2s INTEGER, \
    \level_3s    INTEGER, \
    \level_4s_5s_or_dtms INTEGER, \
    \more_level_4s_5s_or_dtms INTEGER, \
    \new_members INTEGER, \
    \more_new_members INTEGER, \
    \summer_officers_trained INTEGER, \
    \winter_officers_trained INTEGER, \
    \dues_paid_october INTEGER, \
    \dues_paid_april INTEGER, \
    \officer_list_on_time INTEGER, \
    \distinguished_status TEXT, \
    \PRIMARY KEY (club_number, as_of) \
    \);"
