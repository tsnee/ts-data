{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import TextShow (showt)

import PersistenceStore.SQLite (TableName (..), buildLoadMeasurementsQuery)
import Types.ClubNumber (ClubNumber (..))
import Types.District (District (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "ts-data"
    [ testGroup
        "PersistenceStore"
        [ testGroup
            "buildLoadMeasurementsQuery"
            [ testCase "can build a query with empty [ClubMetrics]" $
                fst (buildLoadMeasurementsQuery (TableName "test") (ClubNumber 1) [] Nothing Nothing)
                  @?= "SELECT club_id, metric_id, value, date FROM test WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) ORDER BY date ASC;"
            , testCase "can build a query with singleton [ClubMetrics]" $
                fst (buildLoadMeasurementsQuery (TableName "test") (ClubNumber 1) [toEnum 3] Nothing Nothing)
                  @?= "SELECT club_id, metric_id, value, date FROM test WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) AND metric_id IN (3) ORDER BY date ASC;"
            , testCase "can build a query with multiple [ClubMetrics]" $
                fst
                  (buildLoadMeasurementsQuery (TableName "test") (ClubNumber 1) [toEnum 5, toEnum 7] Nothing Nothing)
                  @?= "SELECT club_id, metric_id, value, date FROM test WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) AND metric_id IN (5,7) ORDER BY date ASC;"
            ]
        ]
    , testGroup
        "Types"
        [ testGroup
            "Serialization"
            [testCase "District is printed as a bare number" $ showt (District 117) @?= "117"]
        ]
    ]
