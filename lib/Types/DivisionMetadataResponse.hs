{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.DivisionMetadataResponse (DivisionMetadataResponse (..)) where

import Autodocodec (Autodocodec (..), HasCodec (..), object, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic)
import Prelude

import Types.AreaMetadataResponse (AreaMetadataResponse)
import Types.District (District)
import Types.Division (Division)

data DivisionMetadataResponse = DivisionMetadataResponse
  { district :: District
  , division :: Division
  , areas :: NonEmpty AreaMetadataResponse
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec DivisionMetadataResponse
instance HasCodec DivisionMetadataResponse where
  codec =
    object "DivisionMetadataResponse" $
      DivisionMetadataResponse
        <$> requiredField "district" "District" .= district
        <*> requiredField "division" "Division" .= division
        <*> requiredField "areas" "Areas contained in this division" .= areas
instance ToSchema DivisionMetadataResponse where
  declareNamedSchema = declareNamedSchemaViaCodec
