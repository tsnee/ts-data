{-# LANGUAGE OverloadedStrings #-}

module Main where

import Katip (LogItem (..), Severity (..), Verbosity (..))
import Network.HTTP.Types (hContentType)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Proxy (..), throwError)
import Servant.API ((:<|>) (..))
import Servant.Server (Application, ServerT, err404, hoistServer, serve)
import UnliftIO (liftIO)

import MonadStack (AppM, runAppM)
import Serve.Class (Api)
import Serve.ClubMeasurement (processClubMeasurementRequest)
import Serve.ClubMetadata (processClubMetadataRequest)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

port :: Int
port = 8080

dcpDb :: DatabaseName
dcpDb = DatabaseName "dcp.sqlite"

server :: ServerT Api AppM
server = processClubMeasurementRequest :<|> processClubMetadataRequest >>= maybe (throwError err404) pure

api :: Proxy Api
api = Proxy

mkApp :: LogItem c => Conf -> c -> Application
mkApp conf ctx = serve api $ hoistServer api (liftIO . runAppM conf ctx) server

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
  let conf = Conf{db = dcpDb, env = "dev", ns = "server", sev = DebugS, v = V3}
  run port $ middleware $ mkApp conf ()
