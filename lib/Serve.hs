{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Serve
  ( AppM
  , DataApi
  , buildIntSeries -- for tests
  , processRequest
  ) where

import Data.List.NonEmpty (NonEmpty (..), groupWith, toList)
import Data.Text (Text)
import Data.Text as T (intercalate, pack, show)
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
  let intSeries = buildIntSeries intMeasurements
      textSeries = buildTextSeries textMeasurements
  pure AppResponse{series = intSeries <> textSeries}

-- | Converts a list of Measurement Int, sorted by metricId, to a Series.
buildIntSeries :: [Measurement Int] -> [Series]
buildIntSeries xs = toSeries IntCodomain <$> (groupWith metricId xs)

-- | Converts a list of Measurement Text, sorted by metricId, to a Series.
buildTextSeries :: [Measurement Text] -> [Series]
buildTextSeries xs = toSeries TextCodomain <$> (groupWith metricId xs)

toSeries :: forall a. ([a] -> Codomain) -> NonEmpty (Measurement a) -> Series
toSeries toCodomain nel@(m :| _) = Series label domain codomain
 where
  label = T.show (toEnum (metricId m) :: ClubMetrics)
  domain = toList $ (formatDbDate . date) <$> nel
  codomain = toCodomain $ toList $ value <$> nel

formatDbDate :: DbDate -> Text
formatDbDate (DbDate day) = T.pack $ formatTime defaultTimeLocale "%F" day
