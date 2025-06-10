module Main where

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.OpenApi.Internal.Utils (encodePretty)
import Data.Proxy (Proxy (..))
import Servant.OpenApi (toOpenApi)
import Prelude

import Serve.Class (Api)

main :: IO ()
main = writeFile "ts-data-openapi.json" $ BL8.unpack $ encodePretty $ toOpenApi (Proxy @Api)
