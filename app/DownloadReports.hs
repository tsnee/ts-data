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

main :: IO ()
main =
  runAppM
    Conf
      { databaseName = DatabaseName "dcp.sqlite"
      , environment = "dev"
      , namespace = "download-reports"
      , severity = InfoS
      , verbosity = V3
      }
    ()
    $ do
      createTables
      downloadClubPerformanceStarting (District 117) (YearMonthDay 2024 7 1)
