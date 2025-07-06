{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (ask)
import Katip (Severity (..), Verbosity (..))
import Katip.Wai (ApplicationT, middleware, runApplication)
import Network.HTTP.Types (hContentType)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Options.Applicative (Parser, auto, help, long, metavar, option, short, showDefault, value)
import Servant
  ( Handler (..)
  , Proxy (..)
  , ServerT
  , hoistServer
  , serve
  , serveDirectoryWebApp
  , (:<|>) (..)
  )
import UnliftIO (MonadUnliftIO (..))

import AppM (AppM, runAppM)
import Options (parseWithConf)
import Serve.Api (Api, AppHandler)
import Serve.ClubMeasurement (processClubMeasurementRequest)
import Serve.ClubMetadata (processMetadataRequestForClub)
import Serve.ClubMetrics (processClubMetricsRequest)
import Serve.GroupMetadata
  ( processMetadataRequestForArea
  , processMetadataRequestForAreas
  , processMetadataRequestForDistrict
  , processMetadataRequestForDivision
  , processMetadataRequestForDivisions
  )
import Types.AppEnv (AppEnv (..))
import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

newtype ServerOptions = ServerOptions {port :: Int}

serverOptions :: Parser ServerOptions
serverOptions =
  ServerOptions
    <$> option
      auto
      ( short 'p'
          <> long "port"
          <> metavar "INT"
          <> help "Server port"
          <> value 8080
          <> showDefault
      )

dcpDb :: DatabaseName
dcpDb = DatabaseName "dcp.sqlite"

server :: ServerT Api AppHandler
server =
  processClubMetricsRequest
    :<|> processMetadataRequestForClub
    :<|> processMetadataRequestForArea
    :<|> processMetadataRequestForAreas
    :<|> processMetadataRequestForDivision
    :<|> processMetadataRequestForDivisions
    :<|> processMetadataRequestForDistrict
    :<|> processClubMeasurementRequest
    :<|> serveDirectoryWebApp "static"

api :: Proxy Api
api = Proxy

natTrans :: forall a. Conf -> AppHandler a -> Handler a
natTrans conf appHandler = Handler $ ExceptT $ runAppM conf () (runExceptT appHandler)

mkApp :: ApplicationT AppM
mkApp req respondF = do
  AppEnv{conf} <- ask
  let corsMiddleware = cors $ const $ Just corsResourcePolicy
      hoistedServer = hoistServer api (natTrans conf) server
      hoistedApp = corsMiddleware $ serve api hoistedServer
  withRunInIO $ \toIO -> hoistedApp req (toIO . respondF)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = [hContentType]
    }

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
  let settings = setPort port $ setHost "::" defaultSettings
      nt = runAppM conf ()
      appWithLogging = middleware NoticeS mkApp
      app = runApplication nt appWithLogging
  runSettings settings app
