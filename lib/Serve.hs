{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Serve (app) where

import Prelude

import Data.Text (Text)
import Servant (Application, Handler, Proxy (..), Server, serve)
import Servant.API (Get, JSON, PlainText, Post, ReqBody, (:<|>) (..), (:>))
import TextShow (showt)

import Types.Request
import Types.Response

type DataApi =
  "measurements" :> "club" :> "integer" :> ReqBody '[JSON] Request :> Post '[JSON] (Response Text Int)
    :<|> "measurements" :> "club" :> "integer" :> Get '[PlainText] Text
    :<|> "measurements" :> "club" :> "text" :> ReqBody '[JSON] Request :> Post '[JSON] (Response Text Int)
    :<|> "measurements" :> "club" :> "text" :> Get '[PlainText] Text

dataApi :: Proxy DataApi
dataApi = Proxy

app :: Application
app = serve dataApi serveData

serveData :: Server DataApi
serveData = processRequest :<|> displayMetrics :<|> processRequest :<|> displayMetrics

processRequest :: Request -> Handler (Response Text Int)
processRequest _ = pure response

dataArray :: [Series Int]
dataArray = [Series {label = "Goal " <> showt (i :: Int), codomain = [0 .. 10]} | i <- [1 .. 10]]

response :: Response Text Int
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

displayMetrics :: Handler Text
displayMetrics = pure "Hi There!"
