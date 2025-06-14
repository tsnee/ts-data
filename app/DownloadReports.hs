{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
  ( Day
  , defaultTimeLocale
  , fromGregorian
  , parseTimeM
  )
import Katip (Severity (..), Verbosity (..))
import Options.Applicative
import Text.Read (readMaybe)

import AppM (runAppM)
import Download (downloadClubPerformanceStarting)
import Options (parseWithConf)
import PersistenceStore.SQLite.Tables (createTables)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))
import Types.District (District (..))

data DownloadOptions = DownloadOptions
  { district :: District
  , startDay :: Day
  }

downloadOptions :: Parser DownloadOptions
downloadOptions =
  DownloadOptions
    <$> option
      readDistrict
      ( long "district"
          <> metavar "INT"
          <> help "District number"
          <> value (District 117)
          <> showDefaultWith (\(District d) -> show d)
      )
    <*> option
      readDay
      ( long "start-day"
          <> metavar "YYYY-MM-DD"
          <> help "Date to start downloading"
          <> value (fromGregorian 2024 7 1)
          <> showDefault
      )
 where
  readDistrict = maybeReader (fmap District . readMaybe)
  readDay = maybeReader (parseTimeM False defaultTimeLocale "%F")

main :: IO ()
main = do
  (conf, DownloadOptions{district, startDay}) <-
    parseWithConf
      Conf
        { databaseName = DatabaseName "dcp.sqlite"
        , environment = "dev"
        , namespace = "download-reports"
        , severity = InfoS
        , verbosity = V3
        }
      downloadOptions
  runAppM conf () $ do
    createTables
    downloadClubPerformanceStarting district startDay
