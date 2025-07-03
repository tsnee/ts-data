{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.FieldParsers (parseEnumField, parseTextField) where

import Data.ByteString.Char8 qualified as BS
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T (pack, show, unpack)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, ResultError (..), returnError)
import Database.SQLite.Simple.Internal (Field (..), gettypename)
import Database.SQLite.Simple.Ok (Ok (..))
import Prelude

safeToEnum :: (Bounded a, Enum a, Eq a, Eq i, Integral i) => i -> Maybe a
safeToEnum x = find ((==) (fromIntegral x) . fromEnum) [minBound .. maxBound]

incompatibleTypeError :: forall a. Typeable a => FieldParser a
incompatibleTypeError f@Field{result} =
  returnError Incompatible f $
    T.unpack $
      "Expected TEXT column for "
        <> T.show (typeRep (Proxy @a))
        <> ", not "
        <> (T.pack . BS.unpack) (gettypename result)

parseEnumField :: forall a. (Bounded a, Enum a, Eq a, Show a, Typeable a) => FieldParser a
parseEnumField f@(Field (SQLInteger x) _) = case safeToEnum x of
  Just e -> Ok e
  Nothing ->
    let typeName = T.show (typeRep (Proxy @a))
        lowerBound = T.show (minBound :: a)
        upperBound = T.show (maxBound :: a)
     in returnError ConversionFailed f $
          T.unpack $
            "Could not parse "
              <> typeName
              <> ": value "
              <> T.show x
              <> " is outside the expected range of "
              <> lowerBound
              <> " to "
              <> upperBound
parseEnumField f = incompatibleTypeError f

parseTextField :: forall a. Typeable a => (Text -> Either Text a) -> FieldParser a
parseTextField parse f@(Field (SQLText t) _) =
  case parse t of
    Right x -> Ok x
    Left err -> returnError ConversionFailed f $ T.unpack err
parseTextField _ f@(Field SQLNull _) =
  returnError UnexpectedNull f $
    T.unpack $
      "Found null instead of " <> T.show (typeRep (Proxy @a)) <> " in database"
parseTextField _ f = incompatibleTypeError f
