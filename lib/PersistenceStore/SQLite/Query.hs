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

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text as T (concat, show, toCaseFold)
import Data.Time (Day)
import Database.SQLite.Simple
  ( Connection
  , FromRow
  , NamedParam (..)
  , Query (..)
  , queryNamed
  )
import Katip (Severity (..), logFM, ls)
import UnliftIO (liftIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

import MonadStack (AppM)
import PersistenceStore.ClubMetrics (ClubMetrics (..))
import PersistenceStore.Measurement (Measurement (..))
import PersistenceStore.SQLite.Class
  ( DatabaseName (..)
  , TableName (..)
  , intMeasurementTable
  , textMeasurementTable
  , withDatabase
  )
import Types.ClubNumber (ClubNumber (..))

loadIntMeasurements
  :: DatabaseName -> ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement Int]
loadIntMeasurements databaseName = loadMeasurements databaseName intMeasurementTable

loadIntMeasurementsWithConnection
  :: Connection -> ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement Int]
loadIntMeasurementsWithConnection conn = loadMeasurementsWithConnection conn intMeasurementTable

loadTextMeasurements
  :: DatabaseName -> ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement Text]
loadTextMeasurements databaseName = loadMeasurements databaseName textMeasurementTable

loadTextMeasurementsWithConnection
  :: Connection -> ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement Text]
loadTextMeasurementsWithConnection conn = loadMeasurementsWithConnection conn textMeasurementTable

loadMeasurements
  :: forall a
   . FromRow (Measurement a)
  => DatabaseName
  -> TableName
  -> ClubNumber
  -> [ClubMetrics]
  -> Maybe Day
  -> Maybe Day
  -> AppM [Measurement a]
loadMeasurements databaseName tableName clubNumber metrics startM endM = withDatabase databaseName $
  \conn -> loadMeasurementsWithConnection conn tableName clubNumber metrics startM endM

loadMeasurementsWithConnection
  :: forall a
   . FromRow (Measurement a)
  => Connection
  -> TableName
  -> ClubNumber
  -> [ClubMetrics]
  -> Maybe Day
  -> Maybe Day
  -> AppM [Measurement a]
loadMeasurementsWithConnection conn tableName clubNumber metrics startM endM = do
  let (query, metricParms) = buildLoadMeasurementsQuery tableName metrics startM endM
  logFM DebugS $ ls $ fromQuery query
  liftIO $
    queryNamed conn query $
      [clubIdParm := clubNumber, startDateParm := startM, endDateParm := endM] <> metricParms

clubIdParm :: Text
clubIdParm = ":clubId"
clubIdParmQ :: Query
clubIdParmQ = ":clubId"
startDateParm :: Text
startDateParm = ":start"
startDateParmQ :: Query
startDateParmQ = ":start"
endDateParm :: Text
endDateParm = ":end"
endDateParmQ :: Query
endDateParmQ = ":end"

metricIdParm :: ClubMetrics -> Text
metricIdParm metric = T.concat [":", T.toCaseFold $ T.show metric]
metricIdParmQ :: ClubMetrics -> Query
metricIdParmQ = unsafeCoerce . metricIdParm

buildLoadMeasurementsQuery
  :: TableName -> [ClubMetrics] -> Maybe Day -> Maybe Day -> (Query, [NamedParam])
buildLoadMeasurementsQuery tableName clubMetrics startM endM = (query, parms)
 where
  queriesAndParms = do
    metric <- clubMetrics
    let subQuery = buildLoadMeasurementsSubQuery tableName metric startM endM
        parm = metricIdParm metric := fromEnum metric
    pure (subQuery, parm)
  (subQueries, parms) = unzip queriesAndParms
  query = mconcat (intersperse " UNION ALL " subQueries) <> " ORDER BY metric_id"

buildLoadMeasurementsSubQuery
  :: TableName -> ClubMetrics -> Maybe Day -> Maybe Day -> Query
buildLoadMeasurementsSubQuery tableName@(TableName tableNameQ) metric startM endM =
  mconcat
    [ "SELECT club_id, metric_id, value, date FROM "
    , tableNameQ
    , " WHERE club_id = "
    , clubIdParmQ
    , " AND metric_id = "
    , metricIdParmQ metric
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
buildDateSubQuery :: TableName -> ClubMetrics -> Maybe Day -> Maybe Day -> Query
buildDateSubQuery (TableName tableNameQ) metric startM endM =
  case (startM, endM) of
    (Just _, Just _) ->
      mconcat
        [ " AND date BETWEEN COALESCE((SELECT MAX(date) FROM "
        , tableNameQ
        , " WHERE club_id = "
        , clubIdParmQ
        , " AND metric_id = "
        , metricIdParmQ metric
        , " AND date <= "
        , startDateParmQ
        , "), :start) AND "
        , endDateParmQ
        ]
    (Just _, Nothing) ->
      mconcat
        [ " AND date >= COALESCE((SELECT MAX(date) FROM "
        , tableNameQ
        , " WHERE club_id = "
        , clubIdParmQ
        , " AND metric_id = "
        , metricIdParmQ metric
        , " AND date <= "
        , startDateParmQ
        , "), :start) AND ("
        , endDateParmQ
        , " IS NULL)"
        ]
    (Nothing, Just _) -> mconcat [" AND ", startDateParmQ, " IS NULL AND date <= ", endDateParmQ]
    (Nothing, Nothing) -> mconcat [" AND ", startDateParmQ, " IS NULL AND ", endDateParmQ, " IS NULL"]
