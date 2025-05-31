{-# LANGUAGE DerivingVia #-}

module Types.Format (Format (..)) where

import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (FromStringShow (..), TextShow, showt)
import Prelude

data Format = CSV | PDF
  deriving Show
  deriving TextShow via FromStringShow Format
instance ToHttpApiData Format where
  toUrlPiece = showt
