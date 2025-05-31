{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Request (Request (..)) where

import Autodocodec (Autodocodec (..), HasCodec, codec, object, optionalField, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Data.Time (Day)
import GHC.Generics (Generic)

import PersistenceStore.ClubMetrics (ClubMetrics)

data Request = Request
  { metrics :: [ClubMetrics]
  , startDate :: Maybe Day
  , endDate :: Maybe Day
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Request)
instance HasCodec Request where
  codec =
    object "Request" $
      Request
        <$> requiredField "metrics" "List of metrics" .= metrics
        <*> optionalField "start_date" "Beginning of date range" .= startDate
        <*> optionalField "end_date" "End of date range" .= endDate
instance ToSchema Request where
  declareNamedSchema = declareNamedSchemaViaCodec
