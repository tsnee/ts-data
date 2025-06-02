{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : PersistenceStore.SQLite
Description : Persistence code specific to the SQLite implementation.
Maintainer  : tomsnee@gmail.com
-}
module PersistenceStore.SQLite
  ( DatabaseName (..)
  , TableName (..)
  , intMeasurementTable
  , loadIntMeasurements
  , loadIntMeasurementsWithConnection
  , loadTextMeasurements
  , loadTextMeasurementsWithConnection
  , saveClubIfNecessary
  , saveIntMeasurement
  , saveReport
  , saveTextMeasurement
  , testDatabase
  , textMeasurementTable
  , withDatabase
  ) where

import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text as T (concat, show, toCaseFold)
import Data.Time (Day)
import Data.Time.Calendar.Month (Month (..))
import Database.SQLite.Simple
  ( Connection
  , FromRow
  , NamedParam (..)
  , Query (..)
  , close
  , executeNamed
  , execute_
  , open
  , queryNamed
  )
import Database.SQLite.Simple.ToField (ToField)
import Katip
import UnliftIO (bracket, liftIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

import MonadStack (AppM)
import PersistenceStore.Analyzer (analyze)
import PersistenceStore.ClubMetrics (ClubMetrics (ReportingMonth))
import PersistenceStore.Measurement (DbDate (..), Measurement (..))
import Types.ClubNumber (ClubNumber (..))
import Types.ClubPerformanceReport
  ( ClubPerformanceRecord (..)
  , ClubPerformanceReport (..)
  , clubNumber
  )

newtype DatabaseName = DatabaseName String
newtype TableName = TableName Query

testDatabase :: DatabaseName
testDatabase = DatabaseName ":memory:"

withDatabase :: forall a. DatabaseName -> (Connection -> AppM a) -> AppM a
withDatabase databaseName = bracket (openDatabase databaseName) (liftIO . close)

openDatabase :: DatabaseName -> AppM Connection
openDatabase (DatabaseName databaseName) = do
  conn <- liftIO $ open databaseName
  liftIO $ execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

intMeasurementTable :: TableName
intMeasurementTable = TableName "int_measurements"

textMeasurementTable :: TableName
textMeasurementTable = TableName "text_measurements"

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
loadMeasurements databaseName tableName clubNumber metrics startM endM = do
  conn <- openDatabase databaseName
  loadMeasurementsWithConnection conn tableName clubNumber metrics startM endM

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
  query = mconcat (intersperse " UNION ALL " subQueries) <> " ORDER BY date ASC;"

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

saveReport :: DatabaseName -> ClubPerformanceReport -> AppM ()
saveReport databaseName report = do
  conn <- openDatabase databaseName
  saveReportWithConnection conn report

saveReportWithConnection :: Connection -> ClubPerformanceReport -> AppM ()
saveReportWithConnection conn ClubPerformanceReport{dayOfRecord, month, records} = traverse_ (saveRecord conn dayOfRecord month) records

saveRecord :: Connection -> Day -> Month -> ClubPerformanceRecord -> AppM ()
saveRecord conn dayOfRecord month record = do
  let clubId = clubNumber record
      monthMetricId = fromEnum ReportingMonth
      monthValue = case month of MkMonth m -> fromInteger m
      date = DbDate dayOfRecord
      reportingMonthMeasurement = Measurement{clubId, metricId = monthMetricId, value = monthValue, date}
      intRows = analyze (clubNumber record) dayOfRecord record
      textRows = analyze (clubNumber record) dayOfRecord record
  saveClubIfNecessary conn clubId
  traverse_ (saveIntMeasurement conn) (reportingMonthMeasurement : intRows)
  traverse_ (saveTextMeasurement conn) textRows

saveClubIfNecessary :: Connection -> ClubNumber -> AppM ()
saveClubIfNecessary conn (ClubNumber clubNumber) =
  liftIO $
    executeNamed
      conn
      "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
      [":clubNumber" := clubNumber]

saveIntMeasurement :: Connection -> Measurement Int -> AppM ()
saveIntMeasurement = saveMeasurement intMeasurementTable

saveTextMeasurement :: Connection -> Measurement Text -> AppM ()
saveTextMeasurement = saveMeasurement textMeasurementTable

{- ORMOLU_DISABLE -}
saveMeasurement :: forall a. ToField a => TableName -> Connection -> Measurement a -> AppM ()
saveMeasurement (TableName tableName) conn Measurement{clubId, metricId, value, date} =
  liftIO $ executeNamed
    conn
    query
    [ ":clubId"   := clubId
    , ":metricId" := metricId
    , ":value"    := value
    , ":date"     := date
    ]
  where
    query = mconcat
      [ "INSERT OR FAIL INTO ", tableName, " (club_id, metric_id, value, date) "
      , "SELECT :clubId, :metricId, :value, :date "
      , "WHERE NOT EXISTS ("
      ,   "SELECT 1 FROM ", tableName
      ,   " WHERE club_id = :clubId "
      ,   "AND metric_id = :metricId AND value = :value "
      ,   "AND date = ("
      ,     "SELECT MAX(date) FROM ", tableName
      ,     " WHERE club_id = :clubId AND metric_id = :metricId AND date < :date"
      ,   ")"
      , ");"
      ]
{- ORMOLU_ENABLE -}
