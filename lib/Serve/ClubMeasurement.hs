{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Serve.ClubMeasurement (buildIntSeries, calculateDateRange, normalize, processClubMeasurementRequest) where

import Control.Monad.Trans (lift)
import Data.List (sort, uncons, unsnoc)
import Data.List.NonEmpty (NonEmpty (..), groupWith, toList)
import Data.Text (Text)
import Data.Text as T (intercalate, pack, show)
import Data.Time (Day, defaultTimeLocale, formatTime, getCurrentTime, utctDay)
import Katip (Severity (..), logFM, ls)
import UnliftIO (liftIO)
import Prelude

import PersistenceStore.DbDate (DbDate (..))
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Query (loadIntMeasurements, loadTextMeasurements)
import Serve.Api (AppHandler)
import Types.ClubMeasurementRequest (ClubMeasurementRequest)
import Types.ClubMeasurementRequest qualified as CMR (ClubMeasurementRequest (..))
import Types.ClubMeasurementResponse (ClubMeasurementResponse (..), Codomain (..), Series (..))
import Types.ClubMetric (ClubMetric (..))

type DateRange = Maybe (DbDate, DbDate)

{- | Takes an optional start date, and optional end date, a default end
date, and a list of DbDates, and returns a pair of DbDates such that
the first is the greater of the start date and the first date in the
list and the second is the end date, or default end date if the end
date is not specified.
Invariants:
1. second param, if supplied, is never before the last date in the fourth param.
2. third param, if second not supplied, is never before the last date in the fourth param.
Note that the first param may or may not precede all dates in the fourth param.
-}
calculateDateRange :: Maybe Day -> Maybe Day -> Day -> [DbDate] -> DateRange
calculateDateRange startM endM defaultEnd dbDates = do
  let sortedDates = sort dbDates
  firstDate <- fst <$> uncons sortedDates
  lastDate <- snd <$> unsnoc sortedDates
  let startDate = case startM of
        Just start -> max firstDate $ DbDate start
        Nothing -> firstDate
      endDate = case endM of
        Just end -> max lastDate $ DbDate end
        Nothing -> max lastDate $ DbDate defaultEnd
  pure (startDate, endDate)

{- | Takes a date range and a possibly-empty list of non-empty lists and
returns a possibly-altered list. If the first date in the range
is equal to the first date in every nested list and the last date in the
range is equal to the last date in every nested list, then the input is
returned unchanged. Otherwise, every sub-list is guaranteed to have its
first measurement's date altered to the start date and an extra
measurement, namely, a copy of the last measurement copied with the last
date.
Invariant: There is no more than one measurement in each sublist whose
date is before the first date in the range.
-}
normalize :: Eq a => DateRange -> [NonEmpty (Measurement a)] -> [NonEmpty (Measurement a)]
normalize Nothing ms = ms
normalize (Just (startDate, endDate)) nestedList = do
  m :| ms <- nestedList
  let newFirstDate = max startDate $ date m
      newFirstMeasurement = m{date = newFirstDate}
      lastMeasurement = maybe newFirstMeasurement snd $ unsnoc ms
      newLastMeasurement = [lastMeasurement{date = endDate} | date lastMeasurement /= endDate]
      newList = newFirstMeasurement :| ms <> newLastMeasurement
  pure newList

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
  today <- liftIO $ utctDay <$> getCurrentTime
  let dates = (date <$> intMeasurements) <> (date <$> textMeasurements)
      dateRange = calculateDateRange startDate endDate today dates
      normalizedIntMeasurements = normalize dateRange $ groupWith metricId intMeasurements
      intSeries = buildIntSeries normalizedIntMeasurements
      normalizedTextMeasurements = normalize dateRange $ groupWith metricId textMeasurements
      textSeries = buildTextSeries normalizedTextMeasurements
  pure ClubMeasurementResponse{clubNumber, series = intSeries <> textSeries}

{- | Converts a list of NonEmpty Measurement Int to a list of Series.
Invariant: List is grouped by metricId.
-}
buildIntSeries :: [NonEmpty (Measurement Int)] -> [Series]
buildIntSeries xs = toSeries IntCodomain <$> xs

{- | Converts a list of NonEmpty Measurement Text, sorted by metricId, to a list of Series.
Invariant: List is grouped by metricId.
-}
buildTextSeries :: [NonEmpty (Measurement Text)] -> [Series]
buildTextSeries xs = toSeries TextCodomain <$> xs

{- | Builds a Series from a non-empty list of measurements.
Invariant: All measurements have the same metricId.
-}
toSeries :: forall a. ([a] -> Codomain) -> NonEmpty (Measurement a) -> Series
toSeries toCodomain nel@(m :| _) = Series label domain codomain
 where
  label = T.show (toEnum (metricId m) :: ClubMetric)
  domain = toList $ formatDbDate . date <$> nel
  codomain = toCodomain $ toList $ value <$> nel

formatDbDate :: DbDate -> Text
formatDbDate (DbDate day) = T.pack $ formatTime defaultTimeLocale "%F" day
