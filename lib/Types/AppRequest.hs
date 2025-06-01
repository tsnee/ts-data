{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.AppRequest (AppRequest (..)) where

import Autodocodec (Autodocodec (..), HasCodec, codec, object, optionalField, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Data.Time (Day)
import GHC.Generics (Generic)

import PersistenceStore.ClubMetrics (ClubMetrics)
import Types.ClubNumber (ClubNumber)

data AppRequest = AppRequest
  { clubNumber :: !ClubNumber
  , metrics :: ![ClubMetrics]
  , startDate :: !(Maybe Day)
  , endDate :: !(Maybe Day)
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec AppRequest
instance HasCodec AppRequest where
  codec =
    object "AppRequest" $
      AppRequest
        <$> requiredField "club_number" "Club number" .= clubNumber
        <*> requiredField "metrics" "Array of club metrics" .= metrics
        <*> optionalField "start_date" "Beginning of date range" .= startDate
        <*> optionalField "end_date" "End of date range" .= endDate
instance ToSchema AppRequest where
  declareNamedSchema = declareNamedSchemaViaCodec
