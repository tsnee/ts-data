{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ProgramYear (ProgramYear (..)) where

import Data.Text qualified as T (show)
import Data.Time (Year)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData, toUrlPiece)
import Prelude

newtype ProgramYear = ProgramYear Year
  deriving (Eq, FromField, Generic, ToField)
instance Show ProgramYear where
  show (ProgramYear year) = mconcat ["Toastmasters year ", show year, " to ", show $ year + 1]
instance ToHttpApiData ProgramYear where
  toUrlPiece (ProgramYear year) = T.show year <> "-" <> T.show (year + 1)
