{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket, try)
import Control.Monad.Except (ExceptT (..))
import Data.Proxy (Proxy (..))
import Katip (LogEnv, closeScribes, runKatipContextT)
import Network.HTTP.Types (hContentType)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant (Application, Handler (..), hoistServer, serve)

import Logging (initLogging)
import MonadStack (AppM)
import Serve (DataApi, processRequest)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = [hContentType]
    }

handle :: LogEnv -> AppM a -> Handler a
handle le app = Handler $ ExceptT $ try $ runKatipContextT le () "handle" app

mkApp :: LogEnv -> Application
mkApp le = do
  serve (Proxy @DataApi) (hoistServer (Proxy @DataApi) (handle le) processRequest)

main :: IO ()
main =
  bracket (initLogging "dev" "server") closeScribes $ \le -> do
    run 8080 $ cors (const (Just corsResourcePolicy)) $ mkApp le
