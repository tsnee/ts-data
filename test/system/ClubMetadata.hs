{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module System.ClubMetadata where

import Data.Time (addDays, getCurrentTime, utctDay)
import Data.Text (Text)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import UnliftIO (liftIO)
import Prelude

import MonadStack (AppM)
import PersistenceStore.Measurement (DbDate(..), Measurement(..))
import PersistenceStore.SQLite.Insert (saveClubIfNecessary, saveIntMeasurement, saveTextMeasurement)
import PersistenceStore.SQLite.Tables (createTablesWithConnection)
import PersistenceStore.ClubMetrics (ClubMetrics(..))
import Serve.ClubMetadata (processClubMetadataRequest)
import System.AppTestCase (AppAssertion, appTestCase)
import Types.ClubMetadataResponse (ClubMetadataResponse(..))
import Types.ClubNumber (ClubNumber(..))
import Types.District (District(..))
import Types.Division (Division(..))



tests :: TestTree
tests =
  testGroup
    "Serve.ClubMetadata"
    [ appTestCase "clubMetadata" "Latest metadata returned" $ \conn -> do
        today <- utctDay <$> liftIO getCurrentTime
        let yesterday = addDays (-1) today
            club = ClubNumber 1
        createTablesWithConnection conn
        saveClubIfNecessary conn club
        -- older measurements
        saveIntMeasurement conn Measurement
          { clubId = club
          , metricId = fromEnum District
          , value = 1 :: Int
          , date = DbDate yesterday
          }
        saveTextMeasurement conn Measurement
          { clubId = club
          , metricId = fromEnum Division
          , value = "A" :: Text
          , date = DbDate yesterday
          }
        saveTextMeasurement conn Measurement
          { clubId = club
          , metricId = fromEnum ClubName
          , value = "Old Club" :: Text
          , date = DbDate yesterday
          }
        -- latest measurements
        saveIntMeasurement conn Measurement
          { clubId = club
          , metricId = fromEnum District
          , value = 2 :: Int
          , date = DbDate today
          }
        saveTextMeasurement conn Measurement
          { clubId = club
          , metricId = fromEnum Division
          , value = "B" :: Text
          , date = DbDate today
          }
        saveTextMeasurement conn Measurement
          { clubId = club
          , metricId = fromEnum ClubName
          , value = "New Club" :: Text
          , date = DbDate today
          }
        result <- processClubMetadataRequest club
        liftIO $ result @?= Just ClubMetadataResponse
          { clubNumber = club
          , clubName = "New Club"
          , district = District 2
          , division = Division 'B'
          }
    ]
