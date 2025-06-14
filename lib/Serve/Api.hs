{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Serve.Api (Api, AppHandler) where

import Control.Monad.Except (ExceptT)
import Servant.API (Capture, Get, JSON, Post, Raw, ReqBody, (:<|>), (:>))
import Servant.Server (ServerError)

import AppM (AppM)
import Types.ClubMeasurementRequest (ClubMeasurementRequest (..))
import Types.ClubMeasurementResponse (ClubMeasurementResponse (..))
import Types.ClubMetadataResponse (ClubMetadataResponse (..))
import Types.ClubMetric (ClubMetric)
import Types.ClubNumber (ClubNumber (..))

{- ORMOLU_DISABLE -}
type Api
  = "measurements"
    :> "club"
    :> ReqBody '[JSON] ClubMeasurementRequest
    :> Post '[JSON] ClubMeasurementResponse
  :<|> "clubs"
    :> Capture "club_number" ClubNumber
    :> Get '[JSON] ClubMetadataResponse
  :<|> "clubmetrics"
    :> Get '[JSON] [ClubMetric]
  :<|> "static"
    :> Raw
{- ORMOLU_ENABLE -}

type AppHandler = ExceptT ServerError AppM
