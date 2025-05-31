{-# LANGUAGE OverloadedStrings #-}

module Types.SqlParsing (parseEnumField, parseTextField) where

import Data.ByteString.Char8 qualified as BS
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, ResultError (..), returnError)
import Database.SQLite.Simple.Internal (Field (..), gettypename)
import Database.SQLite.Simple.Ok (Ok (..))
import TextShow (TextShow, showt)
import TextShow.Data.Typeable qualified ()
import Prelude

safeToEnum :: (Bounded a, Enum a, Eq a, Eq i, Integral i) => i -> Maybe a
safeToEnum x = find ((==) (fromIntegral x) . fromEnum) [minBound .. maxBound]

incompatibleTypeError :: forall a. (TextShow a, Typeable a) => Field -> Ok a
incompatibleTypeError f@Field{result} =
  returnError Incompatible f $
    T.unpack $
      "Expected TEXT column for "
        <> showt (typeRep (Proxy @a))
        <> ", not "
        <> (T.pack . BS.unpack) (gettypename result)

parseEnumField :: forall a. (Bounded a, Enum a, Eq a, TextShow a, Typeable a) => FieldParser a
parseEnumField f@(Field (SQLInteger x) _) = case safeToEnum x of
  Just e -> Ok e
  Nothing ->
    let typeName = showt (typeRep (Proxy @a))
        lowerBound = showt (minBound :: a)
        upperBound = showt (maxBound :: a)
     in returnError ConversionFailed f $
          T.unpack $
            "Could not parse "
              <> typeName
              <> ": value "
              <> showt x
              <> " is outside the expected range of "
              <> lowerBound
              <> " to "
              <> upperBound
parseEnumField f = incompatibleTypeError f

parseTextField :: forall a. TextShow a => Typeable a => (Text -> Maybe a) -> Field -> Ok a
parseTextField parse f@(Field (SQLText t) _) =
  case parse t of
    Just x -> Ok x
    Nothing ->
      returnError ConversionFailed f $
        T.unpack $
          "Could not convert database value " <> t <> " into a " <> showt (typeRep (Proxy @a))
parseTextField _ f@(Field SQLNull _) =
  returnError UnexpectedNull f $
    T.unpack $
      "Found null instead of " <> showt (typeRep (Proxy @a)) <> " in database"
parseTextField _ f = incompatibleTypeError f
