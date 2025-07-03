{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Serve.Api (Api, AppHandler) where

import Control.Monad.Except (ExceptT)
import Data.Time (Day)
import Servant.API (Capture, Get, JSON, Post, QueryParam, Raw, ReqBody, (:<|>), (:>))
import Servant.Server (ServerError)

import AppM (AppM)
import Types.Area (Area (..))
import Types.AreaMetadataResponse (AreaMetadataResponse (..))
import Types.ClubMeasurementRequest (ClubMeasurementRequest (..))
import Types.ClubMeasurementResponse (ClubMeasurementResponse (..))
import Types.ClubMetadataResponse (ClubMetadataResponse (..))
import Types.ClubMetric (ClubMetric)
import Types.ClubNumber (ClubNumber (..))
import Types.District (District (..))
import Types.DistrictMetadataResponse (DistrictMetadataResponse (..))
import Types.Division (Division (..))
import Types.DivisionMetadataResponse (DivisionMetadataResponse (..))

{- ORMOLU_DISABLE -}
type Api
  =    "clubmetrics"
    :> Get '[JSON] [ClubMetric]
  :<|> "clubs"
    :> Capture "club_number" ClubNumber
    :> QueryParam "date" Day
    :> Get '[JSON] ClubMetadataResponse
  :<|> "districts"
    :> Capture "district" District
    :> "areas"
    :> Capture "area" Area
    :> QueryParam "date" Day
    :> Get '[JSON] AreaMetadataResponse
  :<|> "districts"
    :> Capture "district" District
    :> "areas"
    :> QueryParam "date" Day
    :> Get '[JSON] [AreaMetadataResponse]
  :<|> "districts"
    :> Capture "district" District
    :> "divisions"
    :> Capture "division" Division
    :> QueryParam "date" Day
    :> Get '[JSON] DivisionMetadataResponse
  :<|> "districts"
    :> Capture "district" District
    :> "divisions"
    :> QueryParam "date" Day
    :> Get '[JSON] [DivisionMetadataResponse]
  :<|> "districts"
    :> Capture "district" District
    :> QueryParam "date" Day
    :> Get '[JSON] DistrictMetadataResponse
  :<|> "measurements"
    :> "club"
    :> ReqBody '[JSON] ClubMeasurementRequest
    :> Post '[JSON] ClubMeasurementResponse
  :<|> "static"
    :> Raw
{- ORMOLU_ENABLE -}

type AppHandler = ExceptT ServerError AppM
