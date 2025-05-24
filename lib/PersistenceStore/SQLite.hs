{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite (SQLite, databaseName) where

import Prelude

import Database.SQLite.Simple (Query, execute, open)

import PersistenceStore.Class (Persistable, PersistenceType, Retrievable, retrieve, save)
import Types.ClubPerformanceResult (EnhancedClubPerformanceResult)

databaseName :: String
databaseName = "dcp.sqlite"

data SQLite
instance PersistenceType SQLite

instance Persistable SQLite EnhancedClubPerformanceResult IO () where
    save _ cpr = do
        conn <- open databaseName
        execute conn "INSERT INTO clubperformance VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" cpr

instance Retrievable SQLite Query IO (Maybe EnhancedClubPerformanceResult) where
    retrieve _ _ = pure Nothing
