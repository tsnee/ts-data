{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Libs where

import Data.Bifunctor (second)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import PersistenceStore.ClubMetrics (ClubMetrics (..))
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Serve.ClubMeasurement (buildIntSeries)
import Types.ClubMeasurementResponse (Codomain (..), Series (..))
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
    ]
