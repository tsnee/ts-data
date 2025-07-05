{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Libs.Download.MealyMachine where

import Data.Time (Day, dayPeriod, pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Refined (NonNegative, Refined, refineTH, unrefine)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import Prelude

import Download.MealyMachine
  ( MachineConfig
  , MachineInput (..)
  , MachineOutput (..)
  , MachineState (..)
  , formatDay
  , initializeMachine
  , step
  )
import Download.MealyMachine qualified as MC (MachineConfig (..))
import Types.Area (Area (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport (ClubPerformanceRecord (..))
import Types.ClubPerformanceReport qualified as CPR (ClubPerformanceReport (..))
import Types.ClubPerformanceReportDescriptor qualified as CPRD
  ( ClubPerformanceReportDescriptor (..)
  )
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

expectDebug :: [MachineOutput] -> Bool
expectDebug = any isDebug
 where
  isDebug (LogDebug _) = True
  isDebug _ = False

expectNotice :: [MachineOutput] -> Bool
expectNotice = any isNotice
 where
  isNotice (LogNotice _) = True
  isNotice _ = False

expectWarning :: [MachineOutput] -> Bool
expectWarning = any isWarning
 where
  isWarning (LogWarning _) = True
  isWarning _ = False

expectError :: [MachineOutput] -> Bool
expectError = any isError
 where
  isError (LogError _) = True
  isError _ = False

expectSave :: [MachineOutput] -> Bool
expectSave = any isSave
 where
  isSave (Save _) = True
  isSave _ = False

mkCfg :: Day -> Day -> MachineConfig
mkCfg start end =
  MC.MachineConfig
    { MC.district = District 1
    , MC.startDate = start
    , MC.endDate = end
    , MC.maxEmptyDays = $$(refineTH 1)
    , MC.emptyDayCount = $$(refineTH 0)
    , MC.maxFailures = $$(refineTH 1)
    , MC.failureCount = $$(refineTH 0)
    }

tests :: TestTree
tests =
  testGroup
    "Download.MealyMachine"
    [ testCase "Formats days as expected" $ do
        "July 4, 2025" @?= formatDay (YearMonthDay 2025 7 4)
    , testCase "Handles start date and reporting month in same program year" $ do
        let start = YearMonthDay 2024 8 30
            end = YearMonthDay 2024 9 1
            cfg = mkCfg start end
            expected =
              CPRD.ClubPerformanceReportDescriptor
                CSV
                (District 1)
                (YearMonth 2024 7)
                start
                (ProgramYear 2024)
        case initializeMachine cfg of
          (Awaiting cfg' actual, outs) -> do
            District 1 @?= MC.district cfg'
            start @?= MC.startDate cfg'
            end @?= MC.endDate cfg'
            0 @?= unrefine (MC.failureCount cfg')
            expected @?= actual
            assertBool "notice" $ expectNotice outs
          unexpected -> assertFailure $ "Expected (Awaiting ..., [LogNotice ...]) not " <> show unexpected
    , testCase "Handles start date and reporting month in different program years" $ do
        let start = YearMonthDay 2024 7 1
            end = YearMonthDay 2024 7 31
            cfg = mkCfg start end
            expected =
              CPRD.ClubPerformanceReportDescriptor
                CSV
                (District 1)
                (YearMonth 2024 6)
                start
                (ProgramYear 2023)
        case initializeMachine cfg of
          (Awaiting cfg' actual, outs) -> do
            District 1 @?= MC.district cfg'
            start @?= MC.startDate cfg'
            end @?= MC.endDate cfg'
            0 @?= unrefine (MC.failureCount cfg')
            expected @?= actual
            assertBool "notice" $ expectNotice outs
          unexpected -> assertFailure $ "Expected (Awaiting ..., [LogNotice ...]) not " <> show unexpected
    , testCase "Finishes on correct end date" $ do
        let start = YearMonthDay 2024 8 30
            cfg = mkCfg start start
        case initializeMachine cfg of
          (Awaiting cfg' desc, _) -> do
            let report = CPR.ClubPerformanceReport start (dayPeriod start) []
            case step (Awaiting cfg' desc) (DownloadResult (Right report)) of
              (Finished, outs) -> assertBool "notice" $ expectNotice outs
              unexpected -> assertFailure $ "Expected (Finished, [LogNotice ...]) not " <> show unexpected
          unexpected -> assertFailure $ "Expected (Awaiting ..., [...]) not " <> show unexpected
    , testCase "Advances to next month in same program year" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 5 15) (YearMonthDay 2025 5 30) of
          (Awaiting cfg' desc, _) -> do
            let report = CPR.ClubPerformanceReport (YearMonthDay 2025 5 15) (YearMonth 2025 4) []
                expected = desc{CPRD.reportMonth = YearMonth 2025 5, CPRD.programYear = ProgramYear 2024}
            case step (Awaiting cfg' desc) (DownloadResult (Right report)) of
              (Awaiting _ actual, outs) -> do
                expected @?= actual
                assertBool "debug" $ expectDebug outs
              unexpected -> assertFailure $ "Expected (Awaiting ..., [LogDebug ...]) not " <> show unexpected
          unexpected -> assertFailure $ "Expected (Awaiting ..., [...]) not " <> show unexpected
    , testCase "Advances to next month in next program year" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 7 30) (YearMonthDay 2025 7 31) of
          (Awaiting cfg' desc, _) -> do
            let report = CPR.ClubPerformanceReport (YearMonthDay 2025 7 30) (YearMonth 2025 6) []
                expected = desc{CPRD.reportMonth = YearMonth 2025 7, CPRD.programYear = ProgramYear 2025}
            case step (Awaiting cfg' desc) (DownloadResult (Right report)) of
              (Awaiting _ actual, outs) -> do
                expected @?= actual
                assertBool "debug" $ expectDebug outs
              unexpected -> assertFailure $ "Expected (Awaiting ..., [LogDebug ...]) not " <> show unexpected
          unexpected -> assertFailure $ "Expected (Awaiting ..., [...]) not " <> show unexpected
    , testCase "Advances to the next day after an empty download" $ do
        let cfg = mkCfg (YearMonthDay 2025 3 1) (YearMonthDay 2025 3 31)
            cfg' = cfg{MC.emptyDayCount = $$(refineTH 0), MC.maxEmptyDays = $$(refineTH 2)}
        case initializeMachine cfg' of
          (Awaiting actualCfg desc, _) -> do
            let reportingDate = YearMonthDay 2025 3 1
                reportingMonth = YearMonth 2025 3
                report = CPR.ClubPerformanceReport reportingDate reportingMonth []
                expectedEmptyDayCount :: Refined NonNegative Int = $$(refineTH 1)
                expectedReportDate = YearMonthDay 2025 3 2
            case step (Awaiting actualCfg desc) (DownloadResult (Right report)) of
              ( Awaiting
                  MC.MachineConfig{MC.emptyDayCount = actualEmptyDayCount}
                  CPRD.ClubPerformanceReportDescriptor{CPRD.asOf = actualReportDate}
                , outs
                ) -> do
                  assertEqual "emptyDayCount was not incremented" expectedEmptyDayCount actualEmptyDayCount
                  expectedReportDate @?= actualReportDate
                  assertBool "warning" $ expectWarning outs
              unexpected -> assertFailure $ "Expected (Awaiting ..., [LogWarning ...]) not " <> show unexpected
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "Save a good report after a failure and moves to the next day" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5) of
          (Awaiting cfg desc, _) -> do
            let report = CPR.ClubPerformanceReport (YearMonthDay 2025 5 1) (CPRD.reportMonth desc) [sampleRecord]
                expectedAsOf = succ (CPRD.asOf desc)
            case step (Awaiting cfg{MC.failureCount = $$(refineTH 1)} desc) (DownloadResult (Right report)) of
              (Awaiting cfg' desc', outs) -> do
                0 @?= unrefine (MC.failureCount cfg')
                expectedAsOf @?= CPRD.asOf desc'
                assertBool "save" $ expectSave outs
              unexpected -> assertFailure $ "Expected (Awaiting ..., [Save]) not " <> show unexpected
          unexpected -> assertFailure $ "Expected (Awaiting ..., [...]) not " <> show unexpected
    , testCase "Saves the report for the last day" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5) of
          (Awaiting cfg desc, _) -> do
            let report = CPR.ClubPerformanceReport (YearMonthDay 2025 5 5) (YearMonth 2025 5) [sampleRecord]
                (_, outs) = step (Awaiting cfg desc) (DownloadResult (Right report))
            assertBool "save" $ expectSave outs
          unexpected -> assertFailure $ "Expected (Awaiting ..., [...]) not " <> show unexpected
    , testCase "Retries after download errors" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5)
            cfg' = cfg{MC.failureCount = $$(refineTH 0), MC.maxFailures = $$(refineTH 2)}
        case initializeMachine cfg' of
          (Awaiting cfg'' desc, _) -> case step (Awaiting cfg'' desc) (DownloadResult (Left "err")) of
            (Awaiting actualCfg _, outs) -> do
              1 @?= unrefine (MC.failureCount actualCfg)
              assertBool "warning" $ expectWarning outs
            unexpected -> assertFailure $ "Expected (Awaiting ..., [LogWarning ...]) not " <> show unexpected
          unexpected -> assertFailure $ "Expected (Awaiting ..., [...]) not " <> show unexpected
    , testCase "Stops retrying after n failures" $ do
        case initializeMachine (mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 31)) of
          (Awaiting cfg desc, _) -> do
            let cfg' = cfg{MC.failureCount = $$(refineTH 0), MC.maxFailures = $$(refineTH 1)}
            case step (Awaiting cfg' desc) (DownloadResult (Left "boom")) of
              (Errored, outs) -> assertBool "errors" $ expectError outs
              unexpected -> assertFailure $ "Expected (Errored, LogError ...), not " <> show unexpected
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "Fails on bad input" $ do
        let day = YearMonthDay 2025 5 1
            report = CPR.ClubPerformanceReport day (YearMonth 2025 5) []
        case step Initial (DownloadResult (Right report)) of
          (Failed, outs) -> assertBool "error" $ expectError outs
          unexpected -> assertFailure $ "Expected Failed, not " <> show unexpected
    , testCase "Stops retrying after n empty reports" $ do
        let emptyReport = CPR.ClubPerformanceReport (YearMonthDay 2025 5 1) (YearMonth 2025 5) []
        case initializeMachine (mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 10)) of
          (Awaiting cfg desc, _) -> do
            let cfg' = cfg{MC.emptyDayCount = $$(refineTH 0), MC.maxEmptyDays = $$(refineTH 1)}
            case step (Awaiting cfg' desc) (DownloadResult (Right emptyReport)) of
              (Errored, outs) -> assertBool "errors" $ expectError outs
              unexpected -> assertFailure $ "Expected (Errored, LogError ...) not " <> show unexpected
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    ]
