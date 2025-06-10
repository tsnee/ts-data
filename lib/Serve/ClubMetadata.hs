{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.ClubMetadata (processClubMetadataRequest) where

import Data.Time (getCurrentTime, utctDay)
import Data.Text qualified as T (show)
import Katip (Severity(..), logFM, ls)
import UnliftIO (liftIO)
import Prelude hiding (div)

import MonadStack (AppM)
import PersistenceStore.ClubMetrics (ClubMetrics)
import PersistenceStore.ClubMetrics qualified as M (ClubMetrics(..))
import PersistenceStore.Measurement (Measurement(..))
import PersistenceStore.SQLite.Query (loadIntMeasurements, loadTextMeasurements)
import Types.ClubMetadataResponse (ClubMetadataResponse (..))
import Types.ClubNumber (ClubNumber (..))
import Types.District (District (..))
import Types.Division (Division (..))
import Types.Division qualified as D (fromText)

processClubMetadataRequest :: ClubNumber -> AppM (Maybe ClubMetadataResponse)
processClubMetadataRequest clubNumber = do
  today <- liftIO $ utctDay <$> getCurrentTime
  districts <- loadIntMeasurements clubNumber [M.District] (Just today) Nothing
  districtM <- case districts of
    [Measurement{value}] -> pure $ Just value
    [] -> pure Nothing
    unexpected -> do
      logFM ErrorS $ ls $ "Expected list cardinality of 0 or 1, but found " <> T.show unexpected
      pure Nothing
  namesAndDivisions <- loadTextMeasurements clubNumber [M.ClubName, M.Division] (Just today) Nothing
  (nameM, divisionM) <- case namesAndDivisions of
        [Measurement{metricId = m0, value = v0}, Measurement{metricId = m1, value = v1}] ->
          if m0 == fromEnum M.ClubName then pure (Just v0, Just v1)
            else if m0 == fromEnum M.Division then pure (Just v1, Just v0)
              else do
                logFM ErrorS $ ls $ mconcat ["Expected club name and division, but found [metricId ", T.show (toEnum m0 :: ClubMetrics), ", value ", T.show v0, " : metricId ", T.show (toEnum m1 :: ClubMetrics), ", value ", T.show v1, "]"]
                pure (Nothing, Nothing)
        [] ->
          pure (Nothing, Nothing)
        unexpected -> do
          logFM ErrorS $ ls $ "Expected list cardinality of 0 or 2, but found " <> T.show unexpected
          pure (Nothing, Nothing)
  case (nameM, districtM, divisionM) of
    (Just clubName, Just dist, Just div) -> do
      let district = District dist
      division <- case D.fromText div of
        Just d -> pure $ d
        Nothing -> do
          logFM ErrorS $ ls $ mconcat ["When looking up club ID ", T.show clubNumber, ", found division ", div, "."]
          pure DivisionNotAssigned
      pure $ Just ClubMetadataResponse{clubNumber, clubName, district, division}
    (Nothing, Nothing, Nothing) ->
      pure Nothing
    _ -> do
      logFM ErrorS $ ls $ mconcat ["When looking up club ID ", T.show clubNumber, ", found name ", T.show nameM, ", district", T.show districtM, ", division ", T.show divisionM, "."]
      pure Nothing
