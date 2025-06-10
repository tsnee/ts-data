{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubMetadataResponse (ClubMetadataResponse (..)) where

import Autodocodec (Autodocodec (..), HasCodec (..), object, requiredField, (.=))
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

import Types.ClubNumber (ClubNumber)
import Types.District (District)
import Types.Division (Division)

data ClubMetadataResponse = ClubMetadataResponse
  { clubNumber :: !ClubNumber
  , clubName :: !Text
  , division :: !Division
  , district :: !District
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec ClubMetadataResponse
instance HasCodec ClubMetadataResponse where
  codec =
    object "ClubMetadataResponse" $
      ClubMetadataResponse
        <$> requiredField "club_number" "Club number, with or without leading zeros." .= clubNumber
        <*> requiredField "club_name" "Club name" .= clubName
        <*> requiredField "division" "Club division, if assigned" .= division
        <*> requiredField "district" "Club district" .= district
instance ToSchema ClubMetadataResponse where
  declareNamedSchema = declareNamedSchemaViaCodec
