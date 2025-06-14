module Main where

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.OpenApi.Internal.Utils (encodePretty)
import Data.Proxy (Proxy (..))
import Options.Applicative
import Servant.OpenApi (toOpenApi)
import Prelude

import Serve.Api (Api)

newtype DocsOptions = DocsOptions {outputFile :: FilePath}

docsOptions :: Parser DocsOptions
docsOptions =
  DocsOptions
    <$> strOption
      ( long "output-file"
          <> metavar "FILE"
          <> help "Output JSON file"
          <> value "ts-data-openapi.json"
          <> showDefault
      )

main :: IO ()
main = do
  DocsOptions{outputFile} <- execParser $ info (docsOptions <**> helper) fullDesc
  writeFile outputFile $ BL8.unpack $ encodePretty $ toOpenApi (Proxy @Api)
