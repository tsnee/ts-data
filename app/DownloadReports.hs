{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Time (pattern YearMonthDay)
import Katip (Severity (..), Verbosity (..))

import Download (downloadClubPerformanceStarting)
import MonadStack (runAppM)
import PersistenceStore.SQLite.Tables (createTables)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))
import Types.District (District (..))

database :: DatabaseName
database = DatabaseName "dcp.sqlite"

main :: IO ()
main =
  runAppM Conf{db = database, env = "dev", ns = "download-reports", sev = InfoS, v = V3} () $ do
    createTables database
    downloadClubPerformanceStarting (District 117) (YearMonthDay 2024 7 1)
