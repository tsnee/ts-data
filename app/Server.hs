{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Katip (LogItem (..), Severity (..), Verbosity (..))
import Network.HTTP.Types (hContentType)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Options.Applicative
import Servant (Proxy (..))
import Servant.API ((:<|>) (..))
import Servant.Server (Application, Handler (..), ServerT, hoistServer, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)

import AppM (runAppM)
import Options (parseWithConf)
import Serve.Api (Api, AppHandler)
import Serve.ClubMeasurement (processClubMeasurementRequest)
import Serve.ClubMetadata (processClubMetadataRequest)
import Serve.ClubMetrics (processClubMetricsRequest)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

newtype ServerOptions = ServerOptions {port :: Int}

serverOptions :: Parser ServerOptions
serverOptions =
  ServerOptions
    <$> option
      auto
      ( long "port"
          <> metavar "INT"
          <> help "Server port"
          <> value 8080
          <> showDefault
      )

dcpDb :: DatabaseName
dcpDb = DatabaseName "dcp.sqlite"

server :: ServerT Api AppHandler
server =
  processClubMeasurementRequest
    :<|> processClubMetadataRequest
    :<|> processClubMetricsRequest
    :<|> serveDirectoryWebApp "static"

api :: Proxy Api
api = Proxy

natTrans :: forall a c. LogItem c => Conf -> c -> AppHandler a -> Handler a
natTrans conf ctx appHandler = Handler $ ExceptT $ runAppM conf ctx (runExceptT appHandler)

mkApp :: LogItem c => Conf -> c -> Application
mkApp conf ctx = serve api $ hoistServer api (natTrans conf ctx) server

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = [hContentType]
    }

middleware :: Middleware
middleware = cors $ const $ Just corsResourcePolicy

main :: IO ()
main = do
  (conf, ServerOptions{port}) <-
    parseWithConf
      Conf
        { databaseName = dcpDb
        , environment = "dev"
        , namespace = "server"
        , severity = DebugS
        , verbosity = V3
        }
      serverOptions
  run port $ middleware $ mkApp conf ()
