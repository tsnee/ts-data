{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Response (Codomain (..), Response (..), Series (..)) where

import Autodocodec (
  Autodocodec (..),
  HasCodec,
  JSONCodec,
  codec,
  dimapCodec,
  disjointEitherCodec,
  object,
  requiredField,
  (.=),
 )
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Response a = Response
  { domain :: [a]
  , series :: [Series]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec (Response a))
instance HasCodec a => HasCodec (Response a) where
  codec =
    object "Response" $
      Response
        <$> requiredField "domain" "Data, typically plotted on x axis" .= domain
        <*> requiredField "series" "Codomains with labels" .= series
instance (HasCodec a, ToSchema a) => ToSchema (Response a) where
  declareNamedSchema = declareNamedSchemaViaCodec

data Series = Series
  { label :: !Text
  , codomain :: !Codomain
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via Autodocodec Series
instance HasCodec Series where
  codec =
    object "Series" $
      Series
        <$> requiredField "label" "Label for this codomain" .= label
        <*> requiredField "codomain" "Data, typically plotted on y axis" .= codomain
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
