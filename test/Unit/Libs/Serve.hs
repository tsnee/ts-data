{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Libs.Serve where

import Data.Time (pattern YearMonthDay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import PersistenceStore.DbDate (DbDate (..))
import PersistenceStore.Measurement (Measurement (..))
import Serve.ClubMeasurement (buildIntSeries)
import Types.ClubMeasurementResponse (Codomain (..), Series (..))
import Types.ClubMetric (ClubMetric (..))
import Types.ClubNumber (ClubNumber (..))
import Unit.Common.Sorting (sortByDate)

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
                expected @?= actual
            ]
        ]
    ]
