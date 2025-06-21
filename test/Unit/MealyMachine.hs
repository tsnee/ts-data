{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.MealyMachine where

import Data.Time (Day, dayPeriod, pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=))
import Prelude

import Download.MealyMachine
  ( MachineConfig (..)
  , MachineInput (..)
  , MachineOutput (..)
  , MachineState (..)
  , formatDay
  , initializeMachine
  , step
  )
import Types.Area (Area (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport (ClubPerformanceRecord (..), ClubPerformanceReport (..))
import Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..))
import Types.ClubStatus (ClubStatus (..))
import Types.DistinguishedStatus (DistinguishedStatus (..))
import Types.District (District (..))
import Types.Division (Division (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

sampleRecord :: ClubPerformanceRecord
sampleRecord =
  ClubPerformanceRecord
    { district = District 1
    , division = Division 'A'
    , area = Area 1
    , clubNumber = ClubNumber 1
    , clubName = "Test"
    , clubStatus = Active
    , membershipBase = 10
    , activeMembers = 10
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

expectNotice :: [MachineOutput] -> Bool
expectNotice [LogNotice _] = True
expectNotice _ = False

expectDebug :: [MachineOutput] -> Bool
expectDebug [LogDebug _] = True
expectDebug _ = False

expectWarning :: [MachineOutput] -> Bool
expectWarning [LogWarning _] = True
expectWarning _ = False

expectSave :: [MachineOutput] -> Bool
expectSave [LogInfo _, Save _, LogNotice _] = True
expectSave _ = False

expectErrors :: [MachineOutput] -> Bool
expectErrors [LogError _, LogError _] = True
expectErrors _ = False

mkCfg :: Day -> Day -> MachineConfig
mkCfg start end = MachineConfig {district = District 1, startDate = start, endDate = end, failureCount = 0}

tests :: TestTree
tests =
  testGroup
    "Download.MealyMachine"
    [ testCase "formatDay" $ do
        formatDay (YearMonthDay 2025 7 4) @?= "July 4, 2025"
    , testCase "initializeMachine" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 15) (YearMonthDay 2025 5 20)
            (state, outs) = initializeMachine cfg
            expectedDescriptor =
              ClubPerformanceReportDescriptor
                CSV
                (District 1)
                (YearMonth 2025 4)
                (YearMonthDay 2025 5 15)
                (ProgramYear 2024)
        case state of
          Awaiting cfg' descriptor -> do
            district cfg' @?= District 1
            startDate cfg' @?= YearMonthDay 2025 5 15
            endDate cfg' @?= YearMonthDay 2025 5 20
            failureCount cfg' @?= 0
            descriptor @?= expectedDescriptor
          _ -> assertFailure "Expected Awaiting"
        assertBool "notice" $ expectNotice outs
    , testCase "finish on end date" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 10)
            (Awaiting cfg' desc, _) = initializeMachine cfg
            day = YearMonthDay 2025 5 10
            report = ClubPerformanceReport day (dayPeriod day) []
            (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
        case nextState of
          Finished -> pure ()
          _ -> assertFailure "Expected Finished"
        assertBool "notice" $ expectNotice outs
    , testCase "advance to next month" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 15) (YearMonthDay 2025 6 30)
            (Awaiting cfg' desc, _) = initializeMachine cfg
            day = YearMonthDay 2025 6 1
            report = ClubPerformanceReport day (reportMonth desc) []
            (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
            expectedDescriptor = desc { reportMonth = YearMonth 2025 5, programYear = ProgramYear 2025 }
        case nextState of
          Awaiting _ d -> d @?= expectedDescriptor
          _ -> assertFailure "Expected Awaiting"
        assertBool "debug" $ expectDebug outs
    , testCase "give up on day" $ do
        let start = YearMonthDay 2025 3 1
            cfg = mkCfg start (YearMonthDay 2025 3 31)
            (Awaiting cfg' desc, _) = initializeMachine cfg
            day = YearMonthDay 2025 2 1
            report = ClubPerformanceReport day (reportMonth desc) []
            (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
            expectedCfg = cfg' {failureCount = 1}
            expectedDesc = desc {asOf = succ (asOf desc)}
        case nextState of
          Awaiting c d -> do
            failureCount c @?= failureCount expectedCfg
            asOf d @?= asOf expectedDesc
          _ -> assertFailure "Expected Awaiting"
        assertBool "warning" $ expectWarning outs
    , testCase "save report and increment" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5)
            (Awaiting cfg' desc, _) = initializeMachine cfg
            day = YearMonthDay 2025 5 1
            report = ClubPerformanceReport day (reportMonth desc) [sampleRecord]
            (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
            expectedDesc = desc {asOf = succ (asOf desc)}
        case nextState of
          Awaiting c d -> do
            failureCount c @?= 0
            asOf d @?= asOf expectedDesc
          _ -> assertFailure "Expected Awaiting"
        assertBool "save" $ expectSave outs
    , testCase "retry with limits" $ do
        let cfg0 = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5)
            cfg = cfg0 {failureCount = 3}
            (Awaiting _ desc, _) = initializeMachine cfg0
            (nextState, outs) = step (Awaiting cfg desc) (DownloadResult (Left "err"))
        case nextState of
          Awaiting c _ -> failureCount c @?= 4
          _ -> assertFailure "Expected Awaiting"
        assertBool "warning" $ expectWarning outs
    , testCase "retry exceeds limit" $ do
        let cfg0 = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5)
            cfg = cfg0 {failureCount = 4}
            (Awaiting _ desc, _) = initializeMachine cfg0
            (nextState, outs) = step (Awaiting cfg desc) (DownloadResult (Left "boom"))
        case nextState of
          Errored -> pure ()
          _ -> assertFailure "Expected Errored"
        assertBool "errors" $ expectErrors outs
    , testCase "bad input" $ do
        let day = YearMonthDay 2025 5 1
            report = ClubPerformanceReport day (YearMonth 2025 5) []
            (nextState, outs) = step Initial (DownloadResult (Right report))
        case nextState of
          Failed -> pure ()
          _ -> assertFailure "Expected Failed"
        assertBool "error" $ case outs of [LogError _] -> True; _ -> False
    ]
