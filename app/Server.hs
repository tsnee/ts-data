{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant (Application)

import Serve (app)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = [hContentType]
    }

-- [ ("Access-Control-Allow-Origin", "http://localhost:3000")

appWithCorsHeaders :: Application
appWithCorsHeaders = cors (const (Just corsResourcePolicy)) app

main :: IO ()
main = run 8080 appWithCorsHeaders
