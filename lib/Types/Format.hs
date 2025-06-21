{-# LANGUAGE DerivingVia #-}

module Types.Format (Format (..)) where

import Data.Text qualified as T (show)
import Servant.API (ToHttpApiData, toUrlPiece)
import Prelude

data Format = CSV | PDF
  deriving (Eq, Show)
instance ToHttpApiData Format where
  toUrlPiece = T.show
