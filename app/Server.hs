{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Katip (LogItem (..), Severity (..), Verbosity (..))
import Network.HTTP.Types (hContentType)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Proxy (..))
import Servant.API ((:<|>) (..))
import Servant.Server (Application, Handler (..), ServerT, hoistServer, serve)

import MonadStack (runAppM)
import Serve.Class (Api, AppHandler)
import Serve.ClubMeasurement (processClubMeasurementRequest)
import Serve.ClubMetadata (processClubMetadataRequest)
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

port :: Int
port = 8080

dcpDb :: DatabaseName
dcpDb = DatabaseName "dcp.sqlite"

server :: ServerT Api AppHandler
server = processClubMeasurementRequest :<|> processClubMetadataRequest

api :: Proxy Api
api = Proxy

nt :: forall a c. LogItem c => Conf -> c -> AppHandler a -> Handler a
nt conf ctx appHandler = Handler $ ExceptT $ runAppM conf ctx (runExceptT appHandler)

mkApp :: LogItem c => Conf -> c -> Application
mkApp conf ctx = serve api $ hoistServer api (nt conf ctx) server

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
