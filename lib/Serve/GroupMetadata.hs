{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.GroupMetadata
  ( processMetadataRequestForArea
  , processMetadataRequestForAreas
  , processMetadataRequestForDistrict
  , processMetadataRequestForDivision
  , processMetadataRequestForDivisions
  ) where

import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..), groupWith)
import Data.Text (Text)
import Data.Time (Day (..), getCurrentTime, utctDay)
import Katip (Severity (..), logFM, ls)
import Servant (throwError)
import Servant.Server (err404, err500)
import UnliftIO (liftIO)
import Prelude

import PersistenceStore.SQLite.Query (loadClubsFor)
import Serve.Api (AppHandler)
import Types.Area (Area (..), fromInt)
import Types.AreaMetadataResponse (AreaMetadataResponse)
import Types.AreaMetadataResponse qualified as AMR (AreaMetadataResponse (..))
import Types.ClubMetadataResponse (ClubMetadataResponse)
import Types.ClubMetadataResponse qualified as CMR (ClubMetadataResponse (..))
import Types.ClubName (ClubName (..))
import Types.ClubNumber (ClubNumber (..))
import Types.District (District (..))
import Types.DistrictMetadataResponse (DistrictMetadataResponse (..))
import Types.Division (Division, fromText)
import Types.DivisionMetadataResponse (DivisionMetadataResponse)
import Types.DivisionMetadataResponse qualified as VMR (DivisionMetadataResponse (..))

processMetadataRequestForArea
  :: District -> Area -> Maybe Day -> AppHandler AreaMetadataResponse
processMetadataRequestForArea district area Nothing = do
  today <- liftIO $ utctDay <$> getCurrentTime
  processMetadataRequestForArea district area (Just today)
processMetadataRequestForArea district area (Just end) = do
  divisionsAreasClubs <- lift $ loadClubsFor district (Just (Left area)) end
  logFM DebugS $ ls $ "Received " <> show (length divisionsAreasClubs) <> " rows from database."
  case buildAreaMetadataResponse district divisionsAreasClubs of
    Right [] -> throwError err404
    Right [a] -> pure a
    Right unexpected -> do
      logFM ErrorS $ ls $ "Expected a list of 0 or 1 AreaMetadataResponses, found " <> show unexpected
      throwError err500
    Left err -> do
      logFM ErrorS $ ls $ "Database lookup for AreaMetadataResponses failed: " <> err
      throwError err500

processMetadataRequestForAreas
  :: District -> Maybe Day -> AppHandler [AreaMetadataResponse]
processMetadataRequestForAreas district Nothing = do
  logFM InfoS $ ls $ "Received request for current club metadata for all areas of " <> show district
  today <- liftIO $ utctDay <$> getCurrentTime
  logFM DebugS $ ls $ "Using " <> show today <> " to look up club metadata."
  processMetadataRequestForAreas district (Just today)
processMetadataRequestForAreas district (Just end) = do
  divisionsAreasClubs <- lift $ loadClubsFor district Nothing end
  logFM DebugS $ ls $ "Received " <> show (length divisionsAreasClubs) <> " rows from database."
  case buildAreaMetadataResponse district divisionsAreasClubs of
    Right [] -> throwError err404
    Right responses -> pure responses
    Left err -> do
      logFM ErrorS $ ls $ "Database lookup for AreaMetadataResponses failed: " <> err
      throwError err500

processMetadataRequestForDistrict
  :: District -> Maybe Day -> AppHandler DistrictMetadataResponse
processMetadataRequestForDistrict district Nothing = do
  today <- liftIO $ utctDay <$> getCurrentTime
  processMetadataRequestForDistrict district (Just today)
processMetadataRequestForDistrict district (Just end) = do
  divisionsAreasClubs <- lift $ loadClubsFor district Nothing end
  case buildDistrictMetadataResponse district divisionsAreasClubs of
    Right [] -> throwError err404
    Right [a] -> pure a
    Right unexpected -> do
      logFM ErrorS $ ls $ "Expected a list of 0 or 1 DistrictMetadataResponses, found " <> show unexpected
      throwError err500
    Left err -> do
      logFM ErrorS $ ls $ "Database lookup for DistrictMetadataResponses failed: " <> err
      throwError err500

processMetadataRequestForDivision
  :: District -> Division -> Maybe Day -> AppHandler DivisionMetadataResponse
processMetadataRequestForDivision district division Nothing = do
  today <- liftIO $ utctDay <$> getCurrentTime
  processMetadataRequestForDivision district division (Just today)
processMetadataRequestForDivision district division (Just end) = do
  divisionsAreasClubs <- lift $ loadClubsFor district (Just (Right division)) end
  case buildDivisionMetadataResponse district divisionsAreasClubs of
    Right [] -> throwError err404
    Right [a] -> pure a
    Right unexpected -> do
      logFM ErrorS $ ls $ "Expected a list of 0 or 1 DivisionMetadataResponses, found " <> show unexpected
      throwError err500
    Left err -> do
      logFM ErrorS $ ls $ "Database lookup for DivisionMetadataResponses failed: " <> err
      throwError err500

processMetadataRequestForDivisions
  :: District -> Maybe Day -> AppHandler [DivisionMetadataResponse]
processMetadataRequestForDivisions district Nothing = do
  today <- liftIO $ utctDay <$> getCurrentTime
  processMetadataRequestForDivisions district (Just today)
processMetadataRequestForDivisions district (Just end) = do
  divisionsAreasClubs <- lift $ loadClubsFor district Nothing end
  case buildDivisionMetadataResponse district divisionsAreasClubs of
    Right [] -> throwError err404
    Right responses -> pure responses
    Left err -> do
      logFM ErrorS $ ls $ "Database lookup for DivisionMetadataResponses failed: " <> err
      throwError err500

buildDistrictMetadataResponse
  :: District -> [(Int, Text, Int, Text, Day)] -> Either Text [DistrictMetadataResponse]
buildDistrictMetadataResponse district divisionsAreasClubs = do
  vmrs <- buildDivisionMetadataResponse district divisionsAreasClubs
  let vmrsByDistrict = groupWith VMR.district vmrs
      buildDmrFromVmrs (hd :| tl) = DistrictMetadataResponse district (hd :| tl)
  pure $ buildDmrFromVmrs <$> vmrsByDistrict

buildDivisionMetadataResponse
  :: District -> [(Int, Text, Int, Text, Day)] -> Either Text [DivisionMetadataResponse]
buildDivisionMetadataResponse district divisionsAreasClubs = do
  amrs <- buildAreaMetadataResponse district divisionsAreasClubs
  let amrsByDivision = groupWith AMR.division amrs
      buildVmrFromAmrs (hd :| tl) = VMR.DivisionMetadataResponse district (AMR.division hd) (hd :| tl)
  pure $ buildVmrFromAmrs <$> amrsByDivision

buildAreaMetadataResponse
  :: District -> [(Int, Text, Int, Text, Day)] -> Either Text [AreaMetadataResponse]
buildAreaMetadataResponse district divisionsAreasClubs = do
  cmrs <- traverse (buildClubMetadataResponse district) divisionsAreasClubs
  let cmrsByArea = groupWith CMR.area cmrs
      buildAmrFromCmrs (hd :| tl) = AMR.AreaMetadataResponse district (CMR.division hd) (CMR.area hd) (hd :| tl)
  pure $ buildAmrFromCmrs <$> cmrsByArea

buildClubMetadataResponse
  :: District -> (Int, Text, Int, Text, Day) -> Either Text ClubMetadataResponse
buildClubMetadataResponse district (clubNumber, clubName, areaInt, divisionText, _) = do
  area <- fromInt areaInt
  division <- fromText divisionText
  pure $ CMR.ClubMetadataResponse (ClubNumber clubNumber) (ClubName clubName) area division district
