{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubMeasurementResponse (ClubMeasurementResponse (..), Codomain (..), Series (..)) where

import Autodocodec
  ( Autodocodec (..)
  , HasCodec
  , JSONCodec
  , codec
  , dimapCodec
  , disjointEitherCodec
  , object
  , requiredField
  , (.=)
  )
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ClubMeasurementResponse = ClubMeasurementResponse {series :: [Series]}
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec ClubMeasurementResponse
instance HasCodec ClubMeasurementResponse where
  codec =
    object "ClubMeasurementResponse" $
      ClubMeasurementResponse
        <$> requiredField "series" "Array of time series" .= series
instance ToSchema ClubMeasurementResponse where
  declareNamedSchema = declareNamedSchemaViaCodec

data Series = Series
  { label :: !Text
  , domain :: ![Text]
  , codomain :: !Codomain
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Series
instance HasCodec Series where
  codec =
    object "Series" $
      Series
        <$> requiredField "label" "Label for this codomain" .= label
        <*> requiredField "domain" "Data typically plotted on x axis" .= domain
        <*> requiredField "codomain" "Data typically plotted on y axis" .= codomain
instance ToSchema Series where
  declareNamedSchema = declareNamedSchemaViaCodec

data Codomain = IntCodomain [Int] | TextCodomain [Text]
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Codomain
instance HasCodec Codomain where
  codec =
    dimapCodec f g $
      disjointEitherCodec (codec :: JSONCodec [Int]) (codec :: JSONCodec [Text])
   where
    f = \case
      Left i -> IntCodomain i
      Right t -> TextCodomain t
    g = \case
      IntCodomain i -> Left i
      TextCodomain t -> Right t
