{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Serve.Class (Api) where

import Servant.API (Capture, Get, JSON, Post, ReqBody, (:<|>), (:>))
import Prelude

import Types.ClubMeasurementRequest (ClubMeasurementRequest (..))
import Types.ClubMeasurementResponse (ClubMeasurementResponse (..))
import Types.ClubMetadataResponse (ClubMetadataResponse (..))

type Api =
  "measurements"
    :> "club"
    :> ReqBody '[JSON] ClubMeasurementRequest
    :> Post '[JSON] ClubMeasurementResponse
    :<|> "clubs"
      :> Capture "club_number" Int
      :> Get '[JSON] ClubMetadataResponse
