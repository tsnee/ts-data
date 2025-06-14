{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite.Query
Description : SQLite code for querying data.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite.Query
  ( loadIntMeasurements
  , loadIntMeasurementsWithConnection
  , loadTextMeasurements
  , loadTextMeasurementsWithConnection
  ) where

import Control.Monad.Reader (MonadReader)
import Data.List (intersperse)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text as T (concat, show, toCaseFold, unpack)
import Data.Time (Day)
import Database.SQLite.Simple
  ( Connection
  , FromRow
  , NamedParam (..)
  , Query (..)
  , queryNamed
  , execute_
  , open
  , close
  )
import Katip (KatipContext, Severity (..), logFM, ls)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import Prelude

import Control.Monad.Reader (MonadReader)
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Common
  ( TableName (..)
  , intMeasurementTable
  , textMeasurementTable
  )
import Types.AppEnv (AppEnv (..))
import Types.ClubMetric (ClubMetric (..))
import Types.ClubNumber (ClubNumber (..))

loadIntMeasurements
  :: forall m
   . ( KatipContext m
     , MonadIO m
     , MonadReader AppEnv m
     , MonadUnliftIO m
     )
  => ClubNumber
  -> [ClubMetric]
  -> Maybe Day
  -> Maybe Day
  -> m [Measurement Int]
loadIntMeasurements = loadMeasurements intMeasurementTable

loadIntMeasurementsWithConnection
  :: (KatipContext m, MonadIO m, MonadReader AppEnv m)
  => Connection
  -> ClubNumber
  -> [ClubMetric]
  -> Maybe Day
  -> Maybe Day
  -> m [Measurement Int]
loadIntMeasurementsWithConnection conn = loadMeasurementsWithConnection conn intMeasurementTable

loadTextMeasurements
  :: forall m
   . ( KatipContext m
     , MonadIO m
     , MonadReader AppEnv m
     , MonadUnliftIO m
     )
  => ClubNumber
  -> [ClubMetric]
  -> Maybe Day
  -> Maybe Day
  -> m [Measurement Text]
loadTextMeasurements = loadMeasurements textMeasurementTable

loadTextMeasurementsWithConnection
  :: (KatipContext m, MonadIO m, MonadReader AppEnv m)
  => Connection
  -> ClubNumber
  -> [ClubMetric]
  -> Maybe Day
  -> Maybe Day
  -> m [Measurement Text]
loadTextMeasurementsWithConnection conn = loadMeasurementsWithConnection conn textMeasurementTable

loadMeasurements
  :: forall m a
   . ( FromRow (Measurement a)
     , KatipContext m
     , MonadIO m
     , MonadReader AppEnv m
     , MonadUnliftIO m
     )
  => TableName
  -> ClubNumber
  -> [ClubMetric]
  -> Maybe Day
  -> Maybe Day
  -> m [Measurement a]
loadMeasurements tableName clubNumber metrics startM endM =
  withDatabase $ \conn ->
    loadMeasurementsWithConnection
      conn
      tableName
      clubNumber
      metrics
      startM
      endM

loadMeasurementsWithConnection
  :: forall m a
   . (FromRow (Measurement a), KatipContext m, MonadIO m)
  => Connection
  -> TableName
  -> ClubNumber
  -> [ClubMetric]
  -> Maybe Day
  -> Maybe Day
  -> m [Measurement a]
loadMeasurementsWithConnection conn tableName clubNumber metrics startM endM = do
  let (query, metricParams) = buildLoadMeasurementsQuery tableName metrics startM endM
  logFM DebugS $ ls $ fromQuery query
  liftIO $
    queryNamed conn query $
      [clubIdParam := clubNumber, startDateParam := startM, endDateParam := endM] <> metricParams

clubIdParam :: Text
clubIdParam = ":clubId"
clubIdParamQ :: Query
clubIdParamQ = ":clubId"
startDateParam :: Text
startDateParam = ":start"
startDateParamQ :: Query
startDateParamQ = ":start"
endDateParam :: Text
endDateParam = ":end"
endDateParamQ :: Query
endDateParamQ = ":end"

metricIdParam :: ClubMetric -> Text
metricIdParam metric = T.concat [":", T.toCaseFold $ T.show metric]
metricIdParamQ :: ClubMetric -> Query
metricIdParamQ = fromString . T.unpack . metricIdParam

buildLoadMeasurementsQuery
  :: TableName -> [ClubMetric] -> Maybe Day -> Maybe Day -> (Query, [NamedParam])
buildLoadMeasurementsQuery tableName clubMetrics startM endM = (query, params)
 where
  queriesAndParams = do
    metric <- clubMetrics
    let subQuery = buildLoadMeasurementsSubQuery tableName metric startM endM
        param = metricIdParam metric := fromEnum metric
    pure (subQuery, param)
  (subQueries, params) = unzip queriesAndParams
  query = mconcat (intersperse " UNION ALL " subQueries) <> " ORDER BY metric_id"

buildLoadMeasurementsSubQuery
  :: TableName -> ClubMetric -> Maybe Day -> Maybe Day -> Query
buildLoadMeasurementsSubQuery tableName@(TableName tableNameQ) metric startM endM =
  mconcat
    [ "SELECT club_id, metric_id, value, date FROM "
    , tableNameQ
    , " WHERE club_id = "
    , clubIdParamQ
    , " AND metric_id = "
    , metricIdParamQ metric
    , buildDateSubQuery tableName metric startM endM
    ]

{- | Builds SQL to query over a date range, where both start and end are optional.
Four cases:
1. AND date BETWEEN COALESCE((SELECT MAX(date) FROM int_metrics WHERE club_id = :clubId AND metric_id = :metricId AND date <= :start), :start) AND :end
2. AND date >= COALESCE((SELECT MAX(date) FROM int_metrics WHERE club_id = :clubId AND metric_id = :metricId AND date <= :start), :start) AND (:end IS NULL)
3. AND :start IS NULL AND date <= :end
4. AND :start IS NULL AND :end IS NULL
While the ":start IS NULL" serves no purpose in SQL, we choose to include the :start and :end parameters in every case.
Since SQLite errors out if you give it a NamedParam that doesn't exist in the query, it is easier to always provide them.
-}
buildDateSubQuery :: TableName -> ClubMetric -> Maybe Day -> Maybe Day -> Query
buildDateSubQuery (TableName tableNameQ) metric startM endM =
  case (startM, endM) of
    (Just _, Just _) ->
      mconcat
        [ " AND date BETWEEN COALESCE((SELECT MAX(date) FROM "
        , tableNameQ
        , " WHERE club_id = "
        , clubIdParamQ
        , " AND metric_id = "
        , metricIdParamQ metric
        , " AND date <= "
        , startDateParamQ
        , "), :start) AND "
        , endDateParamQ
        ]
    (Just _, Nothing) ->
      mconcat
        [ " AND date >= COALESCE((SELECT MAX(date) FROM "
        , tableNameQ
        , " WHERE club_id = "
        , clubIdParamQ
        , " AND metric_id = "
        , metricIdParamQ metric
        , " AND date <= "
        , startDateParamQ
        , "), :start) AND ("
        , endDateParamQ
        , " IS NULL)"
        ]
    (Nothing, Just _) -> mconcat [" AND ", startDateParamQ, " IS NULL AND date <= ", endDateParamQ]
    (Nothing, Nothing) -> mconcat [" AND ", startDateParamQ, " IS NULL AND ", endDateParamQ, " IS NULL"]
