{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Serve (AppM, DataApi, processRequest) where

import Data.List.NonEmpty (NonEmpty (..), groupBy)
import Data.Text (Text)
import Data.Text as T (intercalate, pack)
import Data.Time (defaultTimeLocale, formatTime)
import Katip (Severity (..), logFM, ls)
import Servant.API (JSON, Post, ReqBody, (:>))
import TextShow (showt)
import TextShow.Data.Time ()
import Prelude

import MonadStack (AppM)
import PersistenceStore.ClubMetrics (ClubMetrics (..))
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import PersistenceStore.SQLite.Class (DatabaseName (..), loadIntMeasurements, loadTextMeasurements)
import Types.AppRequest (AppRequest (..))
import Types.AppResponse (AppResponse (..), Codomain (..), Series (..))

type DataApi = "measurements" :> "club" :> ReqBody '[JSON] AppRequest :> Post '[JSON] AppResponse

processRequest :: DatabaseName -> AppRequest -> AppM AppResponse
processRequest databaseName AppRequest{clubNumber, metrics, startDate, endDate} = do
  logFM DebugS $
    ls $
      "processRequest "
        <> T.intercalate ", " [showt clubNumber, showt metrics, showt startDate, showt endDate]
        <> " called."
  intMeasurements <- loadIntMeasurements databaseName clubNumber metrics startDate endDate
  logFM DebugS $ ls $ "Found " <> showt intMeasurements <> " integer measurements."
  textMeasurements <- loadTextMeasurements databaseName clubNumber metrics startDate endDate
  logFM DebugS $ ls $ "Found " <> showt textMeasurements <> " text measurements."
  let metricsAreEqual Measurement{metricId = m0} Measurement{metricId = m1} = m0 == m1
      intMeasurementsByMetric = groupBy metricsAreEqual intMeasurements
      intSeries = buildIntSeries . IntMeasurements <$> intMeasurementsByMetric
      textMeasurementsByMetric = groupBy metricsAreEqual textMeasurements
      textSeries = buildTextSeries . TextMeasurements <$> textMeasurementsByMetric
  pure AppResponse{series = intSeries <> textSeries}

newtype IntMeasurements = IntMeasurements (NonEmpty (Measurement Int))
newtype TextMeasurements = TextMeasurements (NonEmpty (Measurement Text))

buildIntSeries :: IntMeasurements -> Series
buildIntSeries (IntMeasurements (x :| xs)) = Series{label, domain, codomain}
 where
  metric :: ClubMetrics = toEnum $ metricId x
  label = showt metric
  split m (dateAcc, IntCodomain valueAcc) = (formatDbDate (date m) : dateAcc, IntCodomain (value m : valueAcc))
  split _ (_, TextCodomain _) = undefined
  (domain, codomain) = foldr split ([], IntCodomain []) (x : xs)

buildTextSeries :: TextMeasurements -> Series
buildTextSeries (TextMeasurements (x :| xs)) = Series{label, domain, codomain}
 where
  metric :: ClubMetrics = toEnum $ metricId x
  label = showt metric
  split _ (_, IntCodomain _) = undefined
  split m (dateAcc, TextCodomain valueAcc) = (formatDbDate (date m) : dateAcc, TextCodomain (value m : valueAcc))
  (domain, codomain) = foldr split ([], TextCodomain []) (x : xs)

formatDbDate :: DbDate -> Text
formatDbDate (DbDate day) = T.pack $ formatTime defaultTimeLocale "%F" day
