{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubMeasurementRequest (ClubMeasurementRequest (..)) where

import Autodocodec (Autodocodec (..), HasCodec, codec, object, optionalField, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Data.Time (Day)
import GHC.Generics (Generic)

import PersistenceStore.ClubMetrics (ClubMetric)
import Types.ClubNumber (ClubNumber)

data ClubMeasurementRequest = ClubMeasurementRequest
  { clubNumber :: !ClubNumber
  , metrics :: ![ClubMetric]
  , startDate :: !(Maybe Day)
  , endDate :: !(Maybe Day)
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec ClubMeasurementRequest
instance HasCodec ClubMeasurementRequest where
  codec =
    object "ClubMeasurementRequest" $
      ClubMeasurementRequest
        <$> requiredField "club_number" "Club number" .= clubNumber
        <*> requiredField "metrics" "Array of club metrics" .= metrics
        <*> optionalField "start_date" "Beginning of date range" .= startDate
        <*> optionalField "end_date" "End of date range" .= endDate
instance ToSchema ClubMeasurementRequest where
  declareNamedSchema = declareNamedSchemaViaCodec
