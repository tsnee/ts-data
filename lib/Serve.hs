{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Serve (DataApi, app, dataApi) where

import Data.Text (Text)
import Servant (Application, Handler, Proxy (..), Server, serve)
import Servant.API (JSON, Post, ReqBody, (:>))
import TextShow (showt)
import Prelude

import Types.Request (Request (..))
import Types.Response (Codomain (..), Response (..), Series (..))

type DataApi = "measurements" :> "club" :> ReqBody '[JSON] Request :> Post '[JSON] (Response Text)

dataApi :: Proxy DataApi
dataApi = Proxy

app :: Application
app = serve dataApi serveData

serveData :: Server DataApi
serveData = processRequest

processRequest :: Request -> Handler (Response Text)
processRequest _ = pure response

dataArray :: [Series]
dataArray = [Series{label = "Goal " <> showt (i :: Int), codomain = IntCodomain [0 .. 10]} | i <- [1 .. 10]]

response :: Response Text
response =
  Response
    { domain =
        [ "July"
        , "August"
        , "September"
        , "October"
        , "November"
        , "December"
        , "January"
        , "February"
        , "March"
        , "April"
        , "May"
        ]
    , series = dataArray
    }
