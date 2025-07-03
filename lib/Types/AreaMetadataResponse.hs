{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.AreaMetadataResponse (AreaMetadataResponse (..)) where

import Autodocodec (Autodocodec (..), HasCodec (..), object, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic)
import Prelude

import Types.Area (Area)
import Types.ClubMetadataResponse (ClubMetadataResponse)
import Types.District (District)
import Types.Division (Division)

data AreaMetadataResponse = AreaMetadataResponse
  { district :: District
  , division :: Division
  , area :: Area
  , clubs :: NonEmpty ClubMetadataResponse
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec AreaMetadataResponse
instance HasCodec AreaMetadataResponse where
  codec =
    object "AreaMetadataResponse" $
      AreaMetadataResponse
        <$> requiredField "district" "District number" .= district
        <*> requiredField "division" "Division letter" .= division
        <*> requiredField "area" "Area number" .= area
        <*> requiredField "clubs" "Clubs contained within this area" .= clubs
instance ToSchema AreaMetadataResponse where
  declareNamedSchema = declareNamedSchemaViaCodec
