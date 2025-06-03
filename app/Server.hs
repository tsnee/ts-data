{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket, try)
import Control.Monad.Except (ExceptT (..))
import Data.Proxy (Proxy (..))
import Katip (LogEnv, Severity (..), closeScribes, runKatipContextT)
import Network.HTTP.Types (hContentType)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Application, Handler (..), hoistServer, serve)

import Logging (initLogging)
import MonadStack (AppM)
import PersistenceStore.SQLite.Class (DatabaseName (..))
import Serve (DataApi, processRequest)

port :: Int
port = 8080

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = [hContentType]
    }

handle :: LogEnv -> AppM a -> Handler a
handle le app = Handler $ ExceptT $ try $ runKatipContextT le () "handle" app

mkApp :: DatabaseName -> LogEnv -> Application
mkApp databaseName le = do
  serve (Proxy @DataApi) $ hoistServer (Proxy @DataApi) (handle le) $ processRequest databaseName

main :: IO ()
main =
  bracket (initLogging "dev" "server" DebugS) closeScribes $ \le -> do
    run port $ cors (const (Just corsResourcePolicy)) $ mkApp (DatabaseName "dcp.sqlite") le
