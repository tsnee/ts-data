{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Serve.Class (Api) where

import Servant.API (Capture, Get, JSON, Post, ReqBody, (:<|>), (:>))

import Types.ClubMeasurementRequest (ClubMeasurementRequest (..))
import Types.ClubMeasurementResponse (ClubMeasurementResponse (..))
import Types.ClubMetadataResponse (ClubMetadataResponse (..))
import Types.ClubNumber (ClubNumber (..))

{- ORMOLU DISABLE -}
type Api =
  "measurements"
    :> "club"
    :> ReqBody '[JSON] ClubMeasurementRequest
    :> Post '[JSON] ClubMeasurementResponse
  :<|> "clubs"
    :> Capture "club_number" ClubNumber
    :> Get '[JSON] ClubMetadataResponse
{- ORMOLU ENABLE -}
