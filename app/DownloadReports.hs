{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Time (pattern YearMonthDay)
import Katip (Severity (InfoS))

import Download (downloadClubPerformanceStarting)
import MonadStack (runAppM)
import PersistenceStore.SQLite.Class (DatabaseName (..))
import PersistenceStore.SQLite.Tables (createTables)
import Types.District (District (..))

database :: DatabaseName
database = DatabaseName "dcp.sqlite"

main :: IO ()
main =
  runAppM "dev" "download-reports" () InfoS $ do
    createTables database
    downloadClubPerformanceStarting
      (DatabaseName "dcp.sqlite")
      (District 117)
      (YearMonthDay 2024 7 1)
