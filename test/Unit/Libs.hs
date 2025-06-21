{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Libs where

import Data.Bifunctor (second)
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Data.List (intercalate, sortBy, uncons)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T (show)
import Data.Time (pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, (@?=))
import Prelude

import Download.Parsers (decodeClubReport, parseFooter)
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Serve.ClubMeasurement (buildIntSeries)
import Serve.ClubMetadata (parseNameDivision)
import Types.ClubMeasurementResponse (Codomain (..), Series (..))
import Types.ClubMetric (ClubMetric (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport (ClubPerformanceRecord (..), ClubPerformanceReport (..))

sortByFirst :: [Text] -> [a] -> ([Text], [a])
sortByFirst xs ys = unzip $ (sortBy . comparing) fst $ zip xs ys

sortByDate :: [Series] -> [Series]
sortByDate seriesList = do
  Series{label, domain, codomain} <- seriesList
  let (xs, ys) = case codomain of
        IntCodomain intCodomain -> second IntCodomain $ sortByFirst domain intCodomain
        TextCodomain textCodomain -> second TextCodomain $ sortByFirst domain textCodomain
  pure $ Series label xs ys

tests :: TestTree
tests =
  testGroup
    "Libs"
    [ testGroup
        "Serve"
        [ testGroup
            "buildIntSeries"
            [ testCase "Three row result set" $ do
                let clubId = ClubNumber 2490993
                    rs =
                      [ Measurement
                          { clubId
                          , metricId = fromEnum ActiveMembers
                          , value = 10
                          , date = DbDate (YearMonthDay 2025 5 1)
                          }
                      , Measurement
                          { clubId
                          , metricId = fromEnum ActiveMembers
                          , value = 20
                          , date = DbDate (YearMonthDay 2025 5 3)
                          }
                      , Measurement
                          { clubId
                          , metricId = fromEnum MembershipBase
                          , value = 15
                          , date = DbDate (YearMonthDay 2025 5 1)
                          }
                      ]
                    actual = sortByDate $ buildIntSeries rs
                    expected =
                      [ Series
                          { label = "ActiveMembers"
                          , domain = ["2025-05-01", "2025-05-03"]
                          , codomain = IntCodomain [10, 20]
                          }
                      , Series
                          { label = "MembershipBase"
                          , domain = ["2025-05-01"]
                          , codomain = IntCodomain [15]
                          }
                      ]
                actual @?= expected
            ]
        , testGroup
            "parseNameDivision"
            [ testCase "Empty list" $ do
                let actual = parseNameDivision [] :: Either Text (Maybe Int, Maybe Int)
                    expected = Right (Nothing, Nothing) :: Either Text (Maybe Int, Maybe Int)
                actual @?= expected
            , testCase "Name then division" $ do
                let clubId = ClubNumber 1
                    baseDate = DbDate (YearMonthDay 2025 5 1)
                    name =
                      Measurement{clubId, metricId = fromEnum ClubName, value = "Foo", date = baseDate}
                        :: Measurement Text
                    division =
                      Measurement{clubId, metricId = fromEnum Division, value = "A", date = baseDate} :: Measurement Text
                    actual = parseNameDivision [name, division]
                    expected = Right (Just "Foo", Just "A")
                actual @?= expected
            , testCase "Division then name" $ do
                let clubId = ClubNumber 1
                    baseDate = DbDate (YearMonthDay 2025 5 1)
                    name = Measurement{clubId, metricId = fromEnum ClubName, value = "Foo", date = baseDate}
                    division = Measurement{clubId, metricId = fromEnum Division, value = "A", date = baseDate}
                    actual = parseNameDivision [division, name] :: Either Text (Maybe Text, Maybe Text)
                    expected = Right (Just "Foo", Just "A") :: Either Text (Maybe Text, Maybe Text)
                actual @?= expected
            , testCase "Unexpected metrics" $ do
                let clubId = ClubNumber 1
                    baseDate = DbDate (YearMonthDay 2025 5 1)
                    m0 = Measurement{clubId, metricId = fromEnum ActiveMembers, value = 10, date = baseDate}
                    m1 = Measurement{clubId, metricId = fromEnum MembershipBase, value = 15, date = baseDate}
                    actual = parseNameDivision [m0, m1] :: Either Text (Maybe Int, Maybe Int)
                    expected =
                      Left $
                        mconcat
                          [ "Expected club name and division, but found [metricId "
                          , T.show ActiveMembers
                          , ", value 10 : metricId "
                          , T.show MembershipBase
                          , ", value 15]"
                          ]
                        :: Either Text (Maybe Int, Maybe Int)
                actual @?= expected
            , testCase "Wrong length" $ do
                let clubId = ClubNumber 1
                    baseDate = DbDate (YearMonthDay 2025 5 1)
                    name = Measurement{clubId, metricId = fromEnum ClubName, value = "Foo", date = baseDate}
                    actual = parseNameDivision [name]
                    expected =
                      Left $ "Expected list length of 0 or 2, but found " <> T.show [name]
                        :: Either Text (Maybe Text, Maybe Text)
                actual @?= expected
            ]
        ]
    , testGroup
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
                        :: [String]
                    mkLine xs = BL8.pack $ intercalate "," (show <$> xs) <> "\r\n"
                    header = mkLine headerFields
                    footer = BL8.pack "Month of May, As of 05/01/2025\r\n"
                    csv = header <> footer
                case decodeClubReport csv of
                  Left err -> assertFailure $ show err
                  Right ClubPerformanceReport{month, dayOfRecord, records} -> do
                    month @?= YearMonth 2025 5
                    dayOfRecord @?= YearMonthDay 2025 5 1
                    assertEqual "records" 0 $ length records
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
                        :: [String]
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
                        :: [String]
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
                        :: [String]
                    mkLine xs = BL8.pack $ intercalate "," (show <$> xs) <> "\r\n"
                    footer = BL8.pack "Month of May, As of 05/15/2025\r\n"
                    csv = mconcat [mkLine headerFields, mkLine row1Fields, mkLine row2Fields, footer]
                case decodeClubReport csv of
                  Left err -> assertFailure $ show err
                  Right ClubPerformanceReport{month, dayOfRecord, records} -> do
                    month @?= YearMonth 2025 5
                    dayOfRecord @?= YearMonthDay 2025 5 15
                    assertEqual "records" 2 $ length records
                    case uncons records of
                      Just (ClubPerformanceRecord{clubNumber, clubName}, _) -> do
                        clubNumber @?= ClubNumber 1666
                        clubName @?= "Whangarei Toastmasters Club"
                      Nothing -> assertFailure "decodeClubReport returns empty list"
            ]
        ]
    ]
