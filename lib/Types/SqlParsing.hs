{-# LANGUAGE OverloadedStrings #-}

module Types.SqlParsing (parseTextField) where

import           Prelude

import qualified Data.ByteString.Char8            as BS
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Database.SQLite.Simple           (SQLData (..))
import           Database.SQLite.Simple.FromField (ResultError (..), returnError)
import           Database.SQLite.Simple.Internal  (Field (..), gettypename)
import           Database.SQLite.Simple.Ok        (Ok (..))

parseTextField :: forall a. Typeable a => (T.Text -> Maybe a) -> T.Text -> Field -> Ok a
parseTextField parse fieldName f@Field { result } =
  case result of
    SQLText t  -> case parse t of
      Just x  -> Ok x
      Nothing -> returnError ConversionFailed f $ T.unpack $ "Expected a " <> fieldName <> " but found " <> t
    SQLNull    -> returnError UnexpectedNull  f $ T.unpack $ "Found null instead of " <> fieldName <> " in database"
    unexpected -> returnError Incompatible    f $ T.unpack $ "Expected TEXT column for " <> fieldName <> ", not "
                  <> (T.pack . BS.unpack) (gettypename unexpected)
