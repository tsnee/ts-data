{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Download.MealyMachine
Description : Pure state machine functions for downloading club performance reports.
Maintainer  : tomsnee@gmail.com
-}
module Download.MealyMachine
  ( MachineConfig (..)
  , MachineInput (..)
  , MachineOutput (..)
  , MachineState (..)
  , formatDay
  , increment
  , initializeMachine
  , step
  , zero
  ) where

import Data.Text (Text)
import Data.Text qualified as T (pack, show)
import Data.Time (Day (..), dayPeriod, pattern July)
import Data.Time.Calendar.Month (Month (..), pattern YearMonth)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Katip (LogStr (..), ls)
import Refined (NonNegative, Refined, refine, unrefine)
import Prelude

import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportDescriptor
  ( ClubPerformanceReportDescriptor
      ( ClubPerformanceReportDescriptor
      , asOf
      , format
      , programYear
      , reportMonth
      )
  )
import Types.District (District (..))
import Types.Format (Format (CSV))
import Types.ProgramYear (ProgramYear (..))

impossible :: Refined NonNegative Int
impossible = undefined

zero :: Refined NonNegative Int
zero = case refine 0 of
  Right z -> z
  Left _ -> impossible

increment :: Refined NonNegative Int -> Refined NonNegative Int
increment x = case refine $ succ $ unrefine x of
  Right i -> i
  Left _ -> impossible

data MachineConfig = MachineConfig
  { district :: District
  , startDate :: Day
  , endDate :: Day
  , maxEmptyDays :: Refined NonNegative Int
  , emptyDayCount :: Refined NonNegative Int
  , maxFailures :: Refined NonNegative Int
  , failureCount :: Refined NonNegative Int
  }
  deriving Show

data MachineState
  = Initial
  | Awaiting MachineConfig ClubPerformanceReportDescriptor
  | Finished
  | Errored
  | Failed
  deriving Show

data MachineInput = Initialize MachineConfig | DownloadResult (Either Text ClubPerformanceReport)

data MachineOutput
  = LogDebug LogStr
  | LogInfo LogStr
  | LogNotice LogStr
  | LogWarning LogStr
  | LogError LogStr
  | Save ClubPerformanceReport
  deriving Show

step :: MachineState -> MachineInput -> (MachineState, [MachineOutput])
step Initial (Initialize cfg) = initializeMachine cfg
step (Awaiting cfg descriptor) (DownloadResult (Right report@ClubPerformanceReport{records = []}))
  | dayOfRecord report >= endDate cfg = finish report
  | dayPeriod (dayOfRecord report) > month report = tryFollowingMonth cfg descriptor
  | otherwise = handleEmptyReport cfg descriptor
step (Awaiting cfg descriptor) (DownloadResult (Right report))
  | dayOfRecord report >= endDate cfg = finish report
  | otherwise = saveReportAndIncrementDay cfg descriptor report
step (Awaiting cfg descriptor) (DownloadResult (Left err)) = retryWithLimits cfg descriptor err
step _ _ = (Failed, [LogError "Bad FSM input."])

initializeMachine :: MachineConfig -> (MachineState, [MachineOutput])
initializeMachine cfg = (resultState, output)
 where
  reportDate = startDate cfg
  reportMonth = pred $ dayPeriod reportDate
  YearMonth reportYear reportMonthOfYear = reportMonth
  programYear = ProgramYear $ if reportMonthOfYear < July then pred reportYear else reportYear
  descriptor = ClubPerformanceReportDescriptor CSV (district cfg) reportMonth reportDate programYear
  resultState = Awaiting cfg descriptor
  output = [LogNotice "Mealy machine initialized."]

finish :: ClubPerformanceReport -> (MachineState, [MachineOutput])
finish result = case result of
  ClubPerformanceReport{records = []} -> (Finished, [LogNotice "Finished, nothing to save for the last day."])
  report -> (Finished, [Save report, LogNotice "Finished."])

tryFollowingMonth
  :: MachineConfig -> ClubPerformanceReportDescriptor -> (MachineState, [MachineOutput])
tryFollowingMonth cfg descriptor = (resultState, output)
 where
  nextReportMonth = succ $ reportMonth descriptor
  YearMonth reportYear reportMonthOfYear = nextReportMonth
  updatedProgramYear = ProgramYear $ if reportMonthOfYear < July then pred reportYear else reportYear
  nextDescriptor = descriptor{reportMonth = nextReportMonth, programYear = updatedProgramYear}
  resultState = Awaiting cfg nextDescriptor
  output =
    [ LogDebug $
        ls $
          mconcat
            [ "No records found for "
            , formatMonth $ reportMonth descriptor
            , " on "
            , formatDay $ asOf descriptor
            , " - assume month-end reporting completed the previous day."
            ]
    ]

handleEmptyReport :: MachineConfig -> ClubPerformanceReportDescriptor -> (MachineState, [MachineOutput])
handleEmptyReport cfg descriptor =
  case increment $ emptyDayCount cfg of
    nextEmptyDayCount | nextEmptyDayCount >= maxEmptyDays cfg ->
      let errorMsg = ls $ mconcat ["Giving up after ", T.show nextEmptyDayCount, " consecutive empty reports."]
      in (Errored, [LogError errorMsg])
    nextEmptyDayCount ->
      let nextCfg = cfg{emptyDayCount = nextEmptyDayCount}
          nextReportDate = succ $ asOf descriptor
          nextDescriptor = descriptor{asOf = nextReportDate}
          warningMsg = ls $ mconcat ["After downloading an empty report for ", T.show descriptor, ", trying next day."]
      in (Awaiting nextCfg nextDescriptor, [LogWarning warningMsg])

saveReportAndIncrementDay
  :: MachineConfig
  -> ClubPerformanceReportDescriptor
  -> ClubPerformanceReport
  -> (MachineState, [MachineOutput])
saveReportAndIncrementDay cfg descriptor report = (resultState, output)
 where
  nextReportDate = succ $ asOf descriptor
  nextDescriptor = descriptor{asOf = nextReportDate}
  resultState = Awaiting cfg{failureCount = zero} nextDescriptor
  infoMsg =
    ls $
      mconcat
        [ "Downloaded "
        , T.show (format descriptor)
        , " with "
        , T.show (length (records report))
        , " records for "
        , formatDay (dayOfRecord report)
        , "."
        ]
  output = [LogInfo infoMsg, Save report]

retryWithLimits
  :: MachineConfig -> ClubPerformanceReportDescriptor -> Text -> (MachineState, [MachineOutput])
retryWithLimits cfg descriptor err = (resultState, output)
 where
  nextFailureCount = increment $ failureCount cfg
  warningMsg = ls $ mconcat ["Failed to download ", T.show descriptor, ", retrying."]
  errorMsg = ls $ mconcat ["Giving up after ", T.show nextFailureCount, " consecutive failures."]
  (resultState, output) =
    if nextFailureCount >= maxFailures cfg
      then
        (Errored, [LogError (ls err), LogError errorMsg])
      else
        (Awaiting cfg{failureCount = nextFailureCount} descriptor, [LogWarning warningMsg])

formatMonth :: Month -> Text
formatMonth = T.pack . formatTime defaultTimeLocale "%B %Y"

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%B %-d, %Y"
