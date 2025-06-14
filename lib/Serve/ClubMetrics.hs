{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Serve.ClubMetrics (processClubMetricsRequest) where

import Prelude

import PersistenceStore.ClubMetrics (ClubMetrics (..))
import Serve.Class (AppHandler)

-- | Returns all available 'ClubMetrics' values.
processClubMetricsRequest :: AppHandler [ClubMetrics]
processClubMetricsRequest = pure [minBound .. maxBound]
