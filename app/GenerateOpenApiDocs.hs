module Main where

import Prelude

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.OpenApi.Internal.Utils (encodePretty)
import Servant.OpenApi (toOpenApi)

import Serve (dataApi)

main :: IO ()
main = writeFile "ts-data-openapi.json" $ BL8.unpack $ encodePretty $ toOpenApi dataApi
