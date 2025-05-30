{-# LANGUAGE DerivingVia #-}

module Types.Format (Format (..)) where

import Prelude

import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (FromStringShow (..), TextShow, showt)

data Format = CSV | PDF
  deriving (Show)
  deriving (TextShow) via FromStringShow Format
instance ToHttpApiData Format where
  toUrlPiece = showt
