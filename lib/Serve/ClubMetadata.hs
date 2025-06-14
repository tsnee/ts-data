{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.ClubMetadata (processClubMetadataRequest) where

import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.Text qualified as T (show)
import Data.Time (Day, getCurrentTime, utctDay)
import Katip (Severity (..), logFM, ls)
import Servant (throwError)
import Servant.Server (err404, err500)
import UnliftIO (liftIO)
import Prelude hiding (div)

import PersistenceStore.ClubMetrics (ClubMetric)
import PersistenceStore.ClubMetrics qualified as M (ClubMetric (..))
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Query (loadIntMeasurements, loadTextMeasurements)
import Serve.Class (AppHandler)
import Types.ClubMetadataResponse (ClubMetadataResponse (..))
import Types.ClubNumber (ClubNumber (..))
import Types.District (District (..))
import Types.Division qualified as D (fromText)

processClubMetadataRequest :: ClubNumber -> AppHandler ClubMetadataResponse
processClubMetadataRequest clubNumber = do
  today <- liftIO $ utctDay <$> getCurrentTime
  districtM <- loadDistrict clubNumber today
  (nameM, divisionM) <- loadNameAndDivision clubNumber today
  case (nameM, districtM, divisionM) of
    (Nothing, Nothing, Nothing) -> throwError err404
    (Just clubName, Just dist, Just div) -> clubMetadataFound clubNumber clubName dist div
    _ -> clubMetadataNotFound clubNumber nameM districtM divisionM

loadDistrict :: ClubNumber -> Day -> AppHandler (Maybe Int)
loadDistrict clubNumber today = do
  districts <- lift $ loadIntMeasurements clubNumber [M.District] (Just today) Nothing
  case districts of
    [Measurement{value}] -> pure $ Just value
    [] -> pure Nothing
    unexpected -> do
      logFM ErrorS $ ls $ "Expected list length of 0 or 1, but found " <> T.show unexpected
      throwError err500

loadNameAndDivision :: ClubNumber -> Day -> AppHandler (Maybe Text, Maybe Text)
loadNameAndDivision clubNumber today = do
  namesAndDivisions <-
    lift $ loadTextMeasurements clubNumber [M.ClubName, M.Division] (Just today) Nothing
  case namesAndDivisions of
    [] -> pure (Nothing, Nothing)
    [Measurement{metricId = m0, value = v0}, Measurement{metricId = m1, value = v1}] ->
      let clubNameId = fromEnum M.ClubName
          divisionId = fromEnum M.Division
       in if m0 == clubNameId && m1 == divisionId
            then pure (Just v0, Just v1)
            else
              if m0 == divisionId && m1 == clubNameId
                then pure (Just v1, Just v0)
                else do
                  logFM ErrorS $
                    ls $
                      mconcat
                        [ "Expected club name and division, but found [metricId "
                        , T.show (toEnum m0 :: ClubMetric)
                        , ", value "
                        , T.show v0
                        , " : metricId "
                        , T.show (toEnum m1 :: ClubMetric)
                        , ", value "
                        , T.show v1
                        , "]"
                        ]
                  throwError err500
    unexpected -> do
      logFM ErrorS $ ls $ "Expected list length of 0 or 2, but found " <> T.show unexpected
      throwError err500

clubMetadataFound :: ClubNumber -> Text -> Int -> Text -> AppHandler ClubMetadataResponse
clubMetadataFound clubNumber clubName dist div = do
  division <- case D.fromText div of
    Just d -> pure d
    Nothing -> do
      logFM ErrorS $
        ls $
          mconcat ["When looking up club ID ", T.show clubNumber, ", found division ", div, "."]
      throwError err500
  pure $ ClubMetadataResponse{clubNumber, clubName, district = District dist, division}

clubMetadataNotFound
  :: ClubNumber -> Maybe Text -> Maybe Int -> Maybe Text -> AppHandler ClubMetadataResponse
clubMetadataNotFound clubNumber nameM districtM divisionM = do
  logFM ErrorS $
    ls $
      mconcat
        [ "When looking up club ID "
        , T.show clubNumber
        , ", found name "
        , T.show nameM
        , ", district"
        , T.show districtM
        , ", division "
        , T.show divisionM
        , "."
        ]
  throwError err500
