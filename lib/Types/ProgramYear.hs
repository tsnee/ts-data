{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ProgramYear (ProgramYear (..)) where

import Prelude

import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (TextShow, fromString, showb, showt)

newtype ProgramYear = ProgramYear Int
    deriving (Generic, FromField, ToField)
instance TextShow ProgramYear where
    showb (ProgramYear year) = showb year <> fromString "-" <> showb (year + 1)
instance ToHttpApiData ProgramYear where
    toUrlPiece = showt
