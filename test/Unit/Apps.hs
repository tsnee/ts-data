{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Apps where

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (intercalate)
import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, (@?=))
import Prelude

import Download (decodeClubReport, parseFooter)
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport (ClubPerformanceReport (..), ClubPerformanceRecord (..))
import Data.Time.Calendar.Month (pattern YearMonth)

tests :: TestTree
tests =
  testGroup
    "Download"
    [ testGroup
        "parseFooter"
        [ testCase "Happy path" $ do
            let footer = "Month of Apr, As of 05/01/2025"
                actual = parseFooter footer
                expected = Right (4, YearMonthDay 2025 5 1)
            actual @?= expected
        , testCase "Bad month" $ do
            let footer = "Month of , As of 05/01/2025"
                actual = parseFooter footer
                expected = Left $ "Could not parse month from fragment 'Month of ' of CSV footer '" <> footer <> "'."
            actual @?= expected
        , testCase "Bad day" $ do
            let footer = "Month of Apr, As of 13/01/2025"
                actual = parseFooter footer
                expected =
                  Left $ "Could not parse date from fragment ', As of 13/01/2025' of CSV footer '" <> footer <> "'."
            actual @?= expected
        ]
    , testGroup
        "decodeClubReport"
        [ testCase "Empty report" $ do
            let headerFields =
                  [ "District"
                  , "Division"
                  , "Area"
                  , "Club Number"
                  , "Club Name"
                  , "Club Status"
                  , "Mem. Base"
                  , "Active Members"
                  , "Goals Met"
                  , "Level 1s"
                  , "Level 2s"
                  , "Add. Level 2s"
                  , "Level 3s"
                  , "Level 4s, Level 5s, or DTM award"
                  , "Add. Level 4s, Level 5s, or DTM award"
                  , "New Members"
                  , "Add. New Members"
                  , "Off. Trained Round 1"
                  , "Off. Trained Round 2"
                  , "Mem. dues on time Oct"
                  , "Mem. dues on time Apr"
                  , "Off. List On Time"
                  , "Club Distinguished Status"
                  ]
                mkLine xs = BL8.pack (intercalate "," (map show xs) <> "\r\n")
                header = mkLine headerFields
                footer = BL8.pack "Month of May, As of 05/01/2025\r\n"
                csv = header <> footer
            case decodeClubReport csv of
              Left err -> assertFailure (show err)
              Right ClubPerformanceReport{month, dayOfRecord, records} -> do
                month @?= YearMonth 2025 5
                dayOfRecord @?= YearMonthDay 2025 5 1
                assertEqual "records" 0 (length records)
        , testCase "Two row report" $ do
            let headerFields =
                  [ "District"
                  , "Division"
                  , "Area"
                  , "Club Number"
                  , "Club Name"
                  , "Club Status"
                  , "Mem. Base"
                  , "Active Members"
                  , "Goals Met"
                  , "Level 1s"
                  , "Level 2s"
                  , "Add. Level 2s"
                  , "Level 3s"
                  , "Level 4s, Level 5s, or DTM award"
                  , "Add. Level 4s, Level 5s, or DTM award"
                  , "New Members"
                  , "Add. New Members"
                  , "Off. Trained Round 1"
                  , "Off. Trained Round 2"
                  , "Mem. dues on time Oct"
                  , "Mem. dues on time Apr"
                  , "Off. List On Time"
                  , "Club Distinguished Status"
                  ]
                mkLine xs = BL8.pack (intercalate "," (map show xs) <> "\r\n")
                header = mkLine headerFields
                row1Fields =
                  [ "112"
                  , "K"
                  , "01"
                  , "00001666"
                  , "Whangarei Toastmasters Club"
                  , "Active"
                  , "24"
                  , "18"
                  , "7"
                  , "4"
                  , "2"
                  , "1"
                  , "2"
                  , "1"
                  , "3"
                  , "3"
                  , "0"
                  , "5"
                  , "7"
                  , "1"
                  , "1"
                  , "1"
                  , ""
                  ]
                row2Fields =
                  [ "112"
                  , "K"
                  , "01"
                  , "00940555"
                  , "Toastmasters @ Lunchtime"
                  , "Active"
                  , "14"
                  , "19"
                  , "8"
                  , "5"
                  , "2"
                  , "1"
                  , "0"
                  , "1"
                  , "1"
                  , "4"
                  , "10"
                  , "4"
                  , "4"
                  , "1"
                  , "1"
                  , "1"
                  , "S"
                  ]
                footer = BL8.pack "Month of May, As of 05/15/2025\r\n"
                csv = header <> mkLine row1Fields <> mkLine row2Fields <> footer
            case decodeClubReport csv of
              Left err -> assertFailure (show err)
              Right ClubPerformanceReport{month, dayOfRecord, records} -> do
                month @?= YearMonth 2025 5
                dayOfRecord @?= YearMonthDay 2025 5 15
                assertEqual "records" 2 (length records)
                let ClubPerformanceRecord{clubNumber, clubName} = head records
                clubNumber @?= ClubNumber 1666
                clubName @?= "Whangarei Toastmasters Club"
        ]
    ]
