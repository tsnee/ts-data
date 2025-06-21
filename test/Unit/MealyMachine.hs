{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.MealyMachine where

import Data.Time (Day, dayPeriod, pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
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
mkCfg start end =
  MC.MachineConfig
    { MC.district = District 1
    , MC.startDate = start
    , MC.endDate = end
    , MC.failureCount = 0
    }

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
              CPRD.ClubPerformanceReportDescriptor
                CSV
                (District 1)
                (YearMonth 2025 4)
                (YearMonthDay 2025 5 15)
                (ProgramYear 2024)
        case state of
          Awaiting cfg' descriptor -> do
            MC.district cfg' @?= District 1
            MC.startDate cfg' @?= YearMonthDay 2025 5 15
            MC.endDate cfg' @?= YearMonthDay 2025 5 20
            MC.failureCount cfg' @?= 0
            descriptor @?= expectedDescriptor
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
        assertBool "notice" $ expectNotice outs
    , testCase "finish on end date" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 10) of
          (Awaiting cfg' desc, _) -> do
            let day = YearMonthDay 2025 5 10
                report = CPR.ClubPerformanceReport day (dayPeriod day) []
                (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
            case nextState of
              Finished -> pure ()
              unexpected -> assertFailure $ "Expected Finished, not " <> show unexpected
            assertBool "notice" $ expectNotice outs
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "advance to next month" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 5 15) (YearMonthDay 2025 6 30) of
          (Awaiting cfg' desc, _) -> do
            let report = CPR.ClubPerformanceReport (YearMonthDay 2025 6 1) (CPRD.reportMonth desc) []
                (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
                expectedDescriptor = desc{CPRD.reportMonth = YearMonth 2025 5, CPRD.programYear = ProgramYear 2025}
            case nextState of
              Awaiting _ d -> d @?= expectedDescriptor
              unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
            assertBool "debug" $ expectDebug outs
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "give up on day" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 3 1) (YearMonthDay 2025 3 31) of
          (Awaiting cfg' desc, _) -> do
            let day = YearMonthDay 2025 2 1
                report = CPR.ClubPerformanceReport day (CPRD.reportMonth desc) []
                (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
                expectedCfg = cfg'{MC.failureCount = 1}
                expectedDesc = desc{CPRD.asOf = succ (CPRD.asOf desc)}
            case nextState of
              Awaiting c d -> do
                MC.failureCount c @?= MC.failureCount expectedCfg
                CPRD.asOf d @?= CPRD.asOf expectedDesc
              unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
            assertBool "warning" $ expectWarning outs
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "save report and increment" $ do
        case initializeMachine $ mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5) of
          (Awaiting cfg' desc, _) -> do
            let report = CPR.ClubPerformanceReport (YearMonthDay 2025 5 1) (CPRD.reportMonth desc) [sampleRecord]
                (nextState, outs) = step (Awaiting cfg' desc) (DownloadResult (Right report))
                expectedDesc = desc{CPRD.asOf = succ (CPRD.asOf desc)}
            case nextState of
              Awaiting c d -> do
                MC.failureCount c @?= 0
                CPRD.asOf d @?= CPRD.asOf expectedDesc
              unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
            assertBool "save" $ expectSave outs
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "retry with limits" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5)
            failures = 3
            cfg' = cfg{MC.failureCount = failures}
        case initializeMachine cfg of
          (Awaiting _ desc, _) -> case step (Awaiting cfg' desc) (DownloadResult (Left "err")) of
            (nextState, outs) -> do
              case nextState of
                Awaiting c _ -> MC.failureCount c @?= succ failures
                unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
              assertBool "warning" $ expectWarning outs
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "retry exceeds limit" $ do
        let cfg = mkCfg (YearMonthDay 2025 5 1) (YearMonthDay 2025 5 5)
            cfg' = cfg{MC.failureCount = 4}
        case initializeMachine cfg of
          (Awaiting _ desc, _) -> case step (Awaiting cfg' desc) (DownloadResult (Left "boom")) of
            (nextState, outs) -> do
              case nextState of
                Errored -> pure ()
                unexpected -> assertFailure $ "Expected Errored, not " <> show unexpected
              assertBool "errors" $ expectErrors outs
          unexpected -> assertFailure $ "Expected Awaiting ..., not " <> show unexpected
    , testCase "bad input" $ do
        let day = YearMonthDay 2025 5 1
            report = CPR.ClubPerformanceReport day (YearMonth 2025 5) []
            (nextState, outs) = step Initial (DownloadResult (Right report))
        case nextState of
          Failed -> pure ()
          _ -> assertFailure "Expected Failed"
        assertBool "error" $ case outs of [LogError _] -> True; _ -> False
    ]
