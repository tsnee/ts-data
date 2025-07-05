{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Bifunctor (first)
import Data.Time
  ( Day
  , defaultTimeLocale
  , parseTimeM
  )
import Katip (Severity (..), Verbosity (..))
import Options.Applicative
  ( Parser
  , ReadM
  , auto
  , eitherReader
  , help
  , long
  , maybeReader
  , metavar
  , option
  , short
  , showDefault
  , showDefaultWith
  , value
  )
import Refined (NonNegative, Positive, Predicate, Refined, displayRefineException, refine, refineTH)
import Text.Read (readEither)

import AppM (runAppM)
import Download.Shell (EmptyDayCount, FailureCount, downloadClubPerformanceReportsFrom)
import Options (parseWithConf)
import PersistenceStore.SQLite.Tables (createTables)
import Types.Conf (Conf (..))
import Types.Counts (RequestsPerMinute)
import Types.DatabaseName (DatabaseName (..))
import Types.District (District (..))

data DownloadOptions = DownloadOptions
  { district :: District
  , startDayM :: Maybe Day
  , endDayM :: Maybe Day
  , maxRequestsPerMinute :: RequestsPerMinute
  , maxEmptyDays :: EmptyDayCount
  , maxFailures :: FailureCount
  }

downloadOptions :: Parser DownloadOptions
downloadOptions =
  DownloadOptions
    <$> option
      auto
      ( short 'd'
          <> long "district"
          <> metavar "INT"
          <> help "District number"
          <> value (District 117)
          <> showDefaultWith (\(District d) -> show d)
      )
    <*> option
      readMaybeDay
      ( short 's'
          <> long "start-day"
          <> metavar "YYYY-MM-DD"
          <> help "First report to download, defaulting to the day after the last date in the database."
          <> value Nothing
      )
    <*> option
      readMaybeDay
      ( short 'e'
          <> long "end-day"
          <> metavar "YYYY-MM-DD"
          <> help "Last report to download, defaulting to today."
          <> value Nothing
      )
    <*> option
      readPositiveInt
      ( short 'r'
          <> long "requests-per-minute"
          <> metavar "INT"
          <> help "Max requests to toastmasters.org in one minute"
          <> value $$(refineTH 1)
          <> showDefault
      )
    <*> option
      readNonNegativeInt
      ( short 'p'
          <> long "max-empty-days"
          <> metavar "INT"
          <> help "Max number of consecutive empty reports to download before giving up"
          <> value $$(refineTH 10)
          <> showDefault
      )
    <*> option
      readNonNegativeInt
      ( short 'f'
          <> long "max-failures"
          <> metavar "INT"
          <> help "Max number of consecutive download failures before giving up"
          <> value $$(refineTH 3)
          <> showDefault
      )

readDay :: ReadM Day
readDay = maybeReader (parseTimeM acceptWhitespace defaultTimeLocale "%F")
 where
  acceptWhitespace = True

readMaybeDay :: ReadM (Maybe Day)
readMaybeDay = pure <$> readDay

refineEither :: Predicate p Int => Int -> Either String (Refined p Int)
refineEither = first displayRefineException . refine

parseIntString :: Predicate p Int => String -> Either String (Refined p Int)
parseIntString x = refineEither =<< readEither x

readNonNegativeInt :: ReadM (Refined NonNegative Int)
readNonNegativeInt = eitherReader parseIntString

readPositiveInt :: ReadM (Refined Positive Int)
readPositiveInt = eitherReader parseIntString

main :: IO ()
main = do
  ( conf
    , DownloadOptions{district, startDayM, endDayM, maxRequestsPerMinute, maxEmptyDays, maxFailures}
    ) <-
    parseWithConf
      Conf
        { databaseName = DatabaseName "dcp.sqlite"
        , environment = "dev"
        , namespace = "download-reports"
        , severity = NoticeS
        , verbosity = V3
        }
      downloadOptions
  runAppM conf () $ do
    createTables
    downloadClubPerformanceReportsFrom
      district
      startDayM
      endDayM
      maxRequestsPerMinute
      maxEmptyDays
      maxFailures
