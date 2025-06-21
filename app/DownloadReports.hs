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
  ( Parser
  , auto
  , help
  , long
  , maybeReader
  , metavar
  , option
  , showDefault
  , showDefaultWith
  , value
  )
import Text.Read (readMaybe)

import AppM (runAppM)
import Download.Shell (downloadClubPerformanceReportsFrom)
import Options (parseWithConf)
import PersistenceStore.SQLite.Tables (createTables)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))
import Types.District (District (..))

data DownloadOptions = DownloadOptions
  { district :: District
  , startDay :: Day
  , endDayM :: Maybe Day
  , maxRequestsPerMinute :: Int
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
          <> help "First report to download."
          <> value (fromGregorian 2020 7 1)
          <> showDefault
      )
    <*> option
      readMaybeDay
      ( long "end-day"
          <> metavar "YYYY-MM-DD"
          <> help "Last report to download"
          <> value Nothing
          <> showDefaultWith (maybe "Today" show)
      )
    <*> option
      auto
      ( long "requests-per-minute"
          <> metavar "INT"
          <> help "Max requests to toastmasters.org in one minute"
          <> value 1
          <> showDefault
      )
 where
  acceptWhitespace = True
  readDay = maybeReader (parseTimeM acceptWhitespace defaultTimeLocale "%F")
  readMaybeDay = pure <$> readDay
  readDistrict = maybeReader (fmap District . readMaybe)

main :: IO ()
main = do
  (conf, DownloadOptions{district, startDay, endDayM, maxRequestsPerMinute}) <-
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
    downloadClubPerformanceReportsFrom district startDay endDayM maxRequestsPerMinute
