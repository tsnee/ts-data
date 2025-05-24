module Types.Format (Format (..)) where

import Prelude

import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (TextShow, fromString, showb, showt)

data Format = CSV | PDF
    deriving (Show)
instance TextShow Format where
    showb = fromString . show
instance ToHttpApiData Format where
    toUrlPiece = showt
