{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Serve.ClubMetrics (processClubMetricsRequest) where

import Prelude

import Serve.Api (AppHandler)
import Types.ClubMetric (ClubMetric (..))

-- | Returns all available 'ClubMetrics' values.
processClubMetricsRequest :: AppHandler [ClubMetric]
processClubMetricsRequest = pure [minBound .. maxBound]
