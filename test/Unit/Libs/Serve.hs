{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Libs.Serve where

import Data.Bifunctor (second)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T (show)
import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Serve.ClubMeasurement (buildIntSeries)
import Serve.ClubMetadata (parseNameDivision)
import Types.ClubMeasurementResponse (Codomain (..), Series (..))
import Types.ClubMetric (ClubMetric (..))
import Types.ClubNumber (ClubNumber (..))

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
    "Libs.Serve"
    [ testGroup
        "ClubMeasurement"
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
        ]
    , testGroup
        "ClubMetadata"
        [ testGroup
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
    ]
