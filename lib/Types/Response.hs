{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Response (Response (..), Series (..)) where

import Autodocodec (Autodocodec (..), HasCodec, codec, object, requiredField, (.=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Series b = Series
  { label :: Text
  , codomain :: [b]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec (Series b))
instance HasCodec b => HasCodec (Series b) where
  codec =
    object "Series" $
      Series
        <$> requiredField "label" "Label for this codomain" .= label
        <*> requiredField "codomain" "Data, typically plotted on y axis" .= codomain
data Response a b = Response
  { domain :: [a]
  , series :: [Series b]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec (Response a b))
instance (HasCodec a, HasCodec b) => HasCodec (Response a b) where
  codec =
    object "Response" $
      Response
        <$> requiredField "domain" "Data, typically plotted on x axis" .= domain
        <*> requiredField "series" "Codomains with labels" .= series
