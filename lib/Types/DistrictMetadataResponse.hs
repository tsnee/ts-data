{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.DistrictMetadataResponse (DistrictMetadataResponse (..)) where

import Autodocodec (Autodocodec (..), HasCodec (..), object, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic)
import Prelude

import Types.District (District)
import Types.DivisionMetadataResponse (DivisionMetadataResponse)

data DistrictMetadataResponse = DistrictMetadataResponse
  { district :: District
  , divisions :: NonEmpty DivisionMetadataResponse
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec DistrictMetadataResponse
instance HasCodec DistrictMetadataResponse where
  codec =
    object "DistrictMetadataResponse" $
      DistrictMetadataResponse
        <$> requiredField "district" "District" .= district
        <*> requiredField "divisions" "Divisions contained in this district" .= divisions
instance ToSchema DistrictMetadataResponse where
  declareNamedSchema = declareNamedSchemaViaCodec
