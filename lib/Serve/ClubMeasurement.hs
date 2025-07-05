{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.ClubMeasurement (buildIntSeries, processClubMeasurementRequest) where

import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..), groupWith, toList)
import Data.Text (Text)
import Data.Text as T (intercalate, pack, show)
import Data.Time (defaultTimeLocale, formatTime)
import Katip (Severity (..), logFM, ls)
import Prelude

import PersistenceStore.DbDate (DbDate (..))
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Query (loadIntMeasurements, loadTextMeasurements)
import Serve.Api (AppHandler)
import Types.ClubMeasurementRequest (ClubMeasurementRequest)
import Types.ClubMeasurementRequest qualified as CMR (ClubMeasurementRequest (..))
import Types.ClubMeasurementResponse (ClubMeasurementResponse (..), Codomain (..), Series (..))
import Types.ClubMetric (ClubMetric (..))

processClubMeasurementRequest :: ClubMeasurementRequest -> AppHandler ClubMeasurementResponse
processClubMeasurementRequest CMR.ClubMeasurementRequest{CMR.clubNumber, CMR.metrics, CMR.startDate, CMR.endDate} = do
  logFM DebugS $
    ls $
      "processRequest "
        <> T.intercalate ", " [T.show clubNumber, T.show metrics, T.show startDate, T.show endDate]
        <> " called."
  intMeasurements <- lift $ loadIntMeasurements clubNumber metrics startDate endDate
  logFM DebugS $ ls $ "Found " <> T.show intMeasurements <> " integer measurements."
  textMeasurements <- lift $ loadTextMeasurements clubNumber metrics startDate endDate
  logFM DebugS $ ls $ "Found " <> T.show textMeasurements <> " text measurements."
  let intSeries = buildIntSeries intMeasurements
      textSeries = buildTextSeries textMeasurements
  pure ClubMeasurementResponse{clubNumber, series = intSeries <> textSeries}

-- | Converts a list of Measurement Int, sorted by metricId, to a list of Series.
buildIntSeries :: [Measurement Int] -> [Series]
buildIntSeries xs = toSeries IntCodomain <$> groupWith metricId xs

-- | Converts a list of Measurement Text, sorted by metricId, to a list of Series.
buildTextSeries :: [Measurement Text] -> [Series]
buildTextSeries xs = toSeries TextCodomain <$> groupWith metricId xs

toSeries :: forall a. ([a] -> Codomain) -> NonEmpty (Measurement a) -> Series
toSeries toCodomain nel@(m :| _) = Series label domain codomain
 where
  label = T.show (toEnum (metricId m) :: ClubMetric)
  domain = toList $ formatDbDate . date <$> nel
  codomain = toCodomain $ toList $ value <$> nel

formatDbDate :: DbDate -> Text
formatDbDate (DbDate day) = T.pack $ formatTime defaultTimeLocale "%F" day
