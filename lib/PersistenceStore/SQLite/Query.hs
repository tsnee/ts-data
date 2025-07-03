{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite.Query
Description : SQLite code for querying data.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite.Query
  ( loadClubsFor
  , loadIntMeasurements
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
  )
import Katip (KatipContext, Severity (..), logFM, ls)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import Prelude

import AppM (AppM)
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Common
  ( TableName (..)
  , intMeasurementTable
  , textMeasurementTable
  , withDatabase
  )
import Types.AppEnv (AppEnv (..))
import Types.Area qualified as A (Area (..))
import Types.ClubMetric (ClubMetric (..))
import Types.ClubNumber (ClubNumber (..))
import Types.District qualified as DS (District (..))
import Types.Division qualified as DV (Division (..))

loadClubsFor
  :: DS.District
  -> Maybe (Either A.Area DV.Division)
  -> Day
  -> AppM [(Int, Text, Int, Text, Day)]
loadClubsFor dist levelFilter asOf =
  let (areaFilter, divisionFilter) = case levelFilter of
        Nothing -> (Nothing, Nothing)
        Just (Left area) -> (Just area, Nothing)
        Just (Right division) -> (Nothing, Just division)
   in withDatabase $ \conn ->
        liftIO $
          queryNamed
            conn
            ( mconcat
                [ "WITH last_district AS ( "
                , " SELECT d.club_id, d.value, d.date"
                , " FROM int_measurements d"
                , " JOIN (SELECT club_id, MAX(date) AS date FROM int_measurements"
                , "       WHERE metric_id = "
                , metricIdParamQ District
                , "       AND date <= "
                , endDateParamQ
                , "       GROUP BY club_id) md"
                , "   ON d.club_id = md.club_id AND d.date = md.date"
                , " WHERE d.metric_id = "
                , metricIdParamQ District
                , ") "
                , ", last_area AS ("
                , " SELECT a.club_id, a.value"
                , " FROM int_measurements a"
                , " JOIN (SELECT club_id, MAX(date) AS date FROM int_measurements"
                , "       WHERE metric_id = "
                , metricIdParamQ Area
                , "       AND date <= "
                , endDateParamQ
                , "       GROUP BY club_id) ma"
                , "   ON a.club_id = ma.club_id AND a.date = ma.date"
                , " WHERE a.metric_id = "
                , metricIdParamQ Area
                , ")"
                , ", last_division AS ("
                , " SELECT v.club_id, v.value"
                , " FROM text_measurements v"
                , " JOIN (SELECT club_id, MAX(date) AS date FROM text_measurements"
                , "       WHERE metric_id = "
                , metricIdParamQ Division
                , "       AND date <= "
                , endDateParamQ
                , "       GROUP BY club_id) mv"
                , "   ON v.club_id = mv.club_id AND v.date = mv.date"
                , " WHERE v.metric_id = "
                , metricIdParamQ Division
                , ")"
                , ", last_name AS ("
                , " SELECT n.club_id, n.value, n.date"
                , " FROM text_measurements n"
                , " JOIN (SELECT club_id, MAX(date) AS date FROM text_measurements"
                , "       WHERE metric_id = "
                , metricIdParamQ ClubName
                , "       AND date <= "
                , endDateParamQ
                , "       GROUP BY club_id) mn"
                , "   ON n.club_id = mn.club_id AND n.date = mn.date"
                , " WHERE n.metric_id = "
                , metricIdParamQ ClubName
                , ")"
                , " SELECT ld.club_id, ln.value, la.value, ldiv.value, ln.date"
                , " FROM last_district ld"
                , " JOIN last_name ln ON ln.club_id = ld.club_id"
                , " JOIN last_area la ON la.club_id = ld.club_id"
                , " JOIN last_division ldiv ON ldiv.club_id = ld.club_id"
                , " WHERE ld.value = :districtValue"
                , "   AND (:divisionValue IS NULL OR ldiv.value = :divisionValue)"
                , "   AND (:areaValue IS NULL OR la.value = :areaValue)"
                , " ORDER BY ld.value ASC, ldiv.value ASC, la.value ASC, ld.club_id ASC, ln.date ASC"
                ]
            )
            [ metricIdParam Area := fromEnum Area
            , metricIdParam ClubName := fromEnum ClubName
            , metricIdParam District := fromEnum District
            , metricIdParam Division := fromEnum Division
            , ":areaValue" := areaFilter
            , ":districtValue" := dist
            , ":divisionValue" := divisionFilter
            , endDateParam := asOf
            ]

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
1. (both)    AND date BETWEEN COALESCE((SELECT MAX(date) FROM x_metrics WHERE club_id = :clubId AND metric_id = :metricId AND date <= :start), :start) AND :end
2. (start)   AND date >= COALESCE((SELECT MAX(date) FROM x_metrics WHERE club_id = :clubId AND metric_id = :metricId AND date <= :start), :start) AND (:end IS NULL)
3. (end)     AND :start IS NULL AND date <= :end
4. (neither) AND :start IS NULL AND :end IS NULL
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
        , "), "
        , startDateParamQ
        , ") AND "
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
        , "), "
        , startDateParamQ
        , ") AND ("
        , endDateParamQ
        , " IS NULL)"
        ]
    (Nothing, Just _) -> mconcat [" AND ", startDateParamQ, " IS NULL AND date <= ", endDateParamQ]
    (Nothing, Nothing) -> mconcat [" AND ", startDateParamQ, " IS NULL AND ", endDateParamQ, " IS NULL"]
