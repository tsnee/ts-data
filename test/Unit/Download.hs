{-# LANGUAGE OverloadedStrings #-}

module Unit.Download where

import Data.IORef
import Data.Time (Day, fromGregorian)
import Data.Time.Calendar.Month (Month(..), YearMonth(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import UnliftIO (liftIO)

import Download
  ( DownloadAction(..)
  , DownloadDeps(..)
  , downloadClubPerformance
  )
import Types.Area (Area(..))
import Types.ClubNumber (ClubNumber(..))
import Types.ClubPerformanceReport (ClubPerformanceRecord(..), ClubPerformanceReport(..))
import Types.ClubStatus (ClubStatus(..))
import Types.DistinguishedStatus (DistinguishedStatus(..))
import Types.District (District(..))
import Types.Division (Division(..))

sampleDay :: Day
sampleDay = fromGregorian 2025 5 1

sampleMonth :: Month
sampleMonth = YearMonth 2025 5

sampleRecord :: ClubPerformanceRecord
sampleRecord =
  ClubPerformanceRecord
    { district = District 1
    , division = Division 'A'
    , area = Area 1
    , clubNumber = ClubNumber 1
    , clubName = "test"
    , clubStatus = Active
    , membershipBase = 0
    , activeMembers = 0
    , goalsMet = 0
    , level1s = 0
    , level2s = 0
    , moreLevel2s = 0
    , level3s = 0
    , level4s5sOrDtms = 0
    , moreLevel4s5sOrDtms = 0
    , newMembers = 0
    , moreNewMembers = 0
    , summerOfficersTrained = 0
    , winterOfficersTrained = 0
    , duesPaidOctober = 0
    , duesPaidApril = 0
    , officerListOnTime = 0
    , distinguishedStatus = NotYet
    }

sampleReport :: ClubPerformanceReport
sampleReport =
  ClubPerformanceReport
    { dayOfRecord = sampleDay
    , month = sampleMonth
    , records = [sampleRecord]
    }

stubDeps :: IORef Int -> DownloadDeps
stubDeps ref =
  DownloadDeps
    { fetchReport = \_ -> pure $ Right sampleReport
    , persistReport = \_ -> liftIO $ modifyIORef' ref (+1)
    , currentDay = pure (fromGregorian 2025 5 2)
    }

tests :: TestTree
tests =
  testGroup
    "Download"
    [ testCase "Persists and continues" $ do
        ref <- newIORef 0
        action <- downloadClubPerformance (stubDeps ref) (District 1) sampleDay sampleMonth
        cnt <- readIORef ref
        cnt @?= 1
        action @?= Continue (fromGregorian 2025 5 2) sampleMonth
    , testCase "Done when past run date" $ do
        ref <- newIORef 0
        let emptyDeps =
              DownloadDeps
                { fetchReport = \_ -> pure $ Right sampleReport{records = []}
                , persistReport = \_ -> liftIO $ modifyIORef' ref (+1)
                , currentDay = pure sampleDay
                }
        action <- downloadClubPerformance emptyDeps (District 1) sampleDay sampleMonth
        cnt <- readIORef ref
        cnt @?= 0
        action @?= Done
    ]
