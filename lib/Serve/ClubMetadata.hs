{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.ClubMetadata (processMetadataRequestForClub) where

import Control.Monad.Trans (lift)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T (show)
import Data.Time (Day, getCurrentTime, utctDay)
import Katip (Severity (..), logFM, ls)
import Servant (throwError)
import Servant.Server (err404, err500)
import UnliftIO (liftIO)
import Prelude

import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Query (loadIntMeasurements, loadTextMeasurements)
import Serve.Api (AppHandler)
import Types.Area (Area (..))
import Types.ClubMetadataResponse (ClubMetadataResponse)
import Types.ClubMetadataResponse qualified as CMR (ClubMetadataResponse (..))
import Types.ClubMetric qualified as M (ClubMetric (..))
import Types.ClubName (ClubName (..))
import Types.ClubNumber (ClubNumber (..))
import Types.District (District (..))
import Types.Division (Division, fromText)

processMetadataRequestForClub :: ClubNumber -> Maybe Day -> AppHandler ClubMetadataResponse
processMetadataRequestForClub clubNumber Nothing = do
  today <- liftIO $ utctDay <$> getCurrentTime
  processMetadataRequestForClub clubNumber $ Just today
processMetadataRequestForClub clubNumber (Just day) = do
  (area, district) <- loadAreaAndDistrict clubNumber day
  (clubName, division) <- loadNameAndDivision clubNumber day
  clubMetadataFound clubNumber clubName area division district

loadAreaAndDistrict :: ClubNumber -> Day -> AppHandler (Area, District)
loadAreaAndDistrict clubNumber day = do
  areasAndDistricts <- lift $ loadIntMeasurements clubNumber (M.Area :| [M.District]) (Just day) Nothing
  let areaBeforeDistrict msmt0 msmt1 =
        if fromEnum M.Area < fromEnum M.District
          then compare (metricId msmt0) (metricId msmt1)
          else compare (metricId msmt1) (metricId msmt0)
  case sortBy areaBeforeDistrict areasAndDistricts of
    [] -> throwError err404
    [Measurement{value = a}, Measurement{value = d}] -> pure (Area a, District d)
    unexpected -> do
      logFM ErrorS $
        ls $
          "Expected list of Area/Districts of length 0 or 2, but found " <> T.show unexpected
      throwError err500

loadNameAndDivision :: ClubNumber -> Day -> AppHandler (ClubName, Division)
loadNameAndDivision clubNumber today = do
  namesAndDivisions <-
    lift $ loadTextMeasurements clubNumber (M.ClubName :| [M.Division]) (Just today) Nothing
  let nameBeforeDivision msmt0 msmt1 =
        if fromEnum M.ClubName < fromEnum M.Division
          then compare (metricId msmt0) (metricId msmt1)
          else compare (metricId msmt1) (metricId msmt0)
  case sortBy nameBeforeDivision namesAndDivisions of
    [] -> throwError err404
    [Measurement{value = clubName}, Measurement{value = divisionText}] ->
      case fromText divisionText of
        Right division -> pure (ClubName clubName, division)
        Left err -> do
          logFM ErrorS $ ls $ "Could not parse division from " <> divisionText <> ": " <> err
          throwError err500
    unexpected -> do
      logFM ErrorS $
        ls $
          "Expected list of ClubName/Divisions of length 0 or 2, but found " <> T.show unexpected
      throwError err500

clubMetadataFound
  :: ClubNumber -> ClubName -> Area -> Division -> District -> AppHandler ClubMetadataResponse
clubMetadataFound clubNumber clubName area division district = do
  pure $ CMR.ClubMetadataResponse{CMR.clubNumber, CMR.clubName, CMR.area, CMR.district, CMR.division}
