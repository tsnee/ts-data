{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Area (Area (..), fromInt) where

import Autodocodec (HasCodec, bimapCodec, codec, (<?>))
import Data.Bifunctor (first)
import Data.Csv qualified as CSV
import Data.OpenApi (ToParamSchema)
import Data.Text (Text)
import Data.Text qualified as T (show, unpack)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..), fromField)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Servant.API (FromHttpApiData, parseUrlPiece)
import Prelude

data Area = Area Int | AreaNotAssigned
  deriving (Eq, Read, Show)
  deriving ToParamSchema via Int
instance CSV.FromField Area where
  parseField s
    | s == "0A" = pure AreaNotAssigned
    | otherwise = Area <$> (CSV.parseField s :: CSV.Parser Int)
instance FromField Area where
  fromField (Field (SQLInteger (-1)) _) = pure AreaNotAssigned
  fromField f = Area <$> (fromField f :: Ok Int)
instance FromHttpApiData Area where
  parseUrlPiece = fromInt . read . T.unpack
instance ToField Area where
  toField AreaNotAssigned = SQLInteger (-1)
  toField (Area a) = SQLInteger $ fromIntegral a
instance HasCodec Area where
  codec = bimapCodec dec enc codec <?> "Area number, or 'AreaNotAssigned' if not assigned"
   where
    enc AreaNotAssigned = -1
    enc (Area a) = a
    dec n = first T.unpack $ fromInt n

fromInt :: Int -> Either Text Area
fromInt (-1) = pure AreaNotAssigned
fromInt i
  | i > 0 = pure $ Area i
  | otherwise = Left $ "Invalid area number " <> T.show i
