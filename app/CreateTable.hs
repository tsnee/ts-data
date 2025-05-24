{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Database.SQLite.Simple (execute_, open)

import PersistenceStore.SQLite (databaseName)

main :: IO ()
main = clubperformance

clubperformance :: IO ()
clubperformance = do
    conn <- open databaseName
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS clubperformance ( \
        \dist       INTEGER, \
        \div        CHAR, \
        \area       INTEGER, \
        \clubNumber INTEGER, \
        \clubName   TEXT, \
        \clubStatus TEXT, \
        \membershipBase INTEGER, \
        \activeMembers  INTEGER, \
        \goalsMet   INTEGER, \
        \level1s    INTEGER, \
        \level2s    INTEGER, \
        \moreLevel2s INTEGER, \
        \level3s    INTEGER, \
        \level4s5sOrDtms INTEGER, \
        \moreLevel4s5sOrDtms INTEGER, \
        \newMembers INTEGER, \
        \moreNewMembers INTEGER, \
        \summerOfficersTrained INTEGER, \
        \winterOfficersTrained INTEGER, \
        \duesPaidOctober INTEGER, \
        \duesPaidApril INTEGER, \
        \officerListOnTime INTEGER, \
        \distinguishedStatus TEXT, \
        \asOf       TEXT, \
        \month      INTEGER, \
        \PRIMARY KEY (clubNumber, asOf) \
        \);"
