{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module PersistenceStore.SQLite
  ( TableName (..)
  , buildLoadMeasurementsQuery
  , intMeasurementTable
  , loadIntMeasurements
  , loadTextMeasurements
  , saveReport
  , textMeasurementTable
  , withDatabase
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text as T (intercalate)
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
import TextShow (showt)
import UnliftIO (bracket, liftIO)
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

newtype TableName = TableName Query

databaseName :: String
databaseName = "dcp.sqlite"

withDatabase :: ReaderT Connection AppM a -> AppM a
withDatabase action = bracket openDatabase (liftIO . close) (runReaderT action)

openDatabase :: AppM Connection
openDatabase = do
  conn <- liftIO $ open databaseName
  liftIO $ execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

intMeasurementTable :: TableName
intMeasurementTable = TableName "int_measurements"

textMeasurementTable :: TableName
textMeasurementTable = TableName "text_measurements"

loadIntMeasurements
  :: ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement Int]
loadIntMeasurements = loadMeasurements intMeasurementTable

loadTextMeasurements
  :: ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement Text]
loadTextMeasurements = loadMeasurements textMeasurementTable

loadMeasurements
  :: forall a
   . FromRow (Measurement a)
  => TableName -> ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> AppM [Measurement a]
loadMeasurements tableName clubNumber metrics startM endM = do
  conn <- openDatabase
  let (textQuery, textParms) = buildLoadMeasurementsQuery tableName clubNumber metrics startM endM
  liftIO $ queryNamed conn textQuery textParms

buildLoadMeasurementsQuery
  :: TableName -> ClubNumber -> [ClubMetrics] -> Maybe Day -> Maybe Day -> (Query, [NamedParam])
buildLoadMeasurementsQuery (TableName tableNameQ) clubNumber metrics startM endM = (query, parms)
 where
  query =
    mconcat
      [ "SELECT club_id, metric_id, value, date FROM "
      , tableNameQ
      , " WHERE club_id = :clubId AND (:start IS NULL OR date >= :start) AND (:end IS NULL OR date <= :end) "
      , metricsClause
      , "ORDER BY date ASC;"
      ]
  metricIds = showt . fromEnum <$> metrics
  metricsClause
    | null metrics = Query ""
    | otherwise = Query $ "AND metric_id IN (" <> T.intercalate "," metricIds <> ") "
  parms = [":clubId" := clubNumber, ":start" := startM, ":end" := endM]

saveReport :: ClubPerformanceReport -> AppM ()
saveReport ClubPerformanceReport{dayOfRecord, month, records} = do
  conn <- openDatabase
  traverse_ (saveRecord conn dayOfRecord month) records

saveRecord :: Connection -> Day -> Month -> ClubPerformanceRecord -> AppM ()
saveRecord conn dayOfRecord month record = do
  liftIO $
    executeNamed
      conn
      "INSERT OR IGNORE INTO clubs(id) VALUES (:clubNumber);"
      [":clubNumber" := clubNumber record]
  let clubId = clubNumber record
      monthMetricId = fromEnum ReportingMonth
      monthValue = case month of MkMonth m -> fromInteger m
      date = DbDate dayOfRecord
      reportingMonthMeasurement = Measurement{clubId, metricId = monthMetricId, value = monthValue, date}
      intRows = analyze (clubNumber record) dayOfRecord record
      textRows = analyze (clubNumber record) dayOfRecord record
  traverse_ (saveIntMeasurement conn) (reportingMonthMeasurement : intRows)
  traverse_ (saveTextMeasurement conn) textRows

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
