{-# LANGUAGE DataKinds, DerivingVia, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Download (CsvOctetStream (..), download, downloadClubPerformanceStarting) where

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv (decodeByName)
import Data.Either.Combinators (maybeToRight)
import Data.List (unsnoc)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( Day (..)
  , MonthOfYear
  , dayPeriod
  , diffDays
  , periodFirstDay
  , periodLastDay
  , pattern July
  )
import Data.Time.Calendar.Month (Month (..), pattern YearMonth)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector (toList)
import Katip
import Network.HTTP.Client (managerModifyRequest, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Accept, Capture, Get, MimeUnrender (..), OctetStream, QueryParam, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import TextShow (showt)
import UnliftIO (liftIO)
import Prelude

import Logging (initLogging)
import MonadStack (AppM)
import PersistenceStore.SQLite (saveReport)
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportSpec qualified as CPRS
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

type ClubPerformanceAPI =
  Capture "programYear" ProgramYear
    :> "export.aspx"
    :> QueryParam "type" Format
    :> QueryParam "report" CPRS.ClubPerformanceReportSpec
    :> Get '[CsvOctetStream] ClubPerformanceReport

clubPerformanceApi :: Proxy ClubPerformanceAPI
clubPerformanceApi = Proxy

downloadClubPerformanceApi
  :: ProgramYear
  -> Maybe Format
  -> Maybe CPRS.ClubPerformanceReportSpec
  -> ClientM ClubPerformanceReport
downloadClubPerformanceApi = client clubPerformanceApi

formatMonth :: Month -> Text
formatMonth = T.pack . formatTime defaultTimeLocale "%B %Y"

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%B %-d, %Y"

downloadClubPerformanceStarting :: District -> Month -> Maybe Day -> AppM ()
downloadClubPerformanceStarting district startMonth Nothing = startFromFirstReportingDate district startMonth $ periodFirstDay startMonth
downloadClubPerformanceStarting district startMonth (Just dayOfRecord) = do
  result <- reportFromDayOfRecord district startMonth dayOfRecord
  case result of
    Left err -> logFM ErrorS $ ls err
    Right ClubPerformanceReport{records = []}
      | dayPeriod dayOfRecord == succ startMonth -> do
          logFM InfoS $ ls $ "No records found for day of record " <> formatDay dayOfRecord <> "."
          downloadClubPerformanceStarting district (succ startMonth) (Just dayOfRecord)
      | otherwise ->
          logFM WarningS $
            ls $
              "Could not download data for "
                <> formatMonth startMonth
                <> " by "
                <> formatDay dayOfRecord
                <> ", giving up."
    Right report -> do
      saveReport report
      downloadClubPerformanceStarting district startMonth $ Just $ succ dayOfRecord

startFromFirstReportingDate :: District -> Month -> Day -> AppM ()
startFromFirstReportingDate district startMonth dayOfRecord = do
  result <- reportFromDayOfRecord district startMonth dayOfRecord
  case result of
    Left err -> logFM ErrorS $ ls err
    Right ClubPerformanceReport{records = []} -> do
      logFM InfoS $
        ls $
          "Month-end processing for "
            <> formatMonth startMonth
            <> " not complete by "
            <> formatDay dayOfRecord
            <> "."
      startFromFirstReportingDate district startMonth $ succ dayOfRecord
    Right report -> do
      saveReport report
      downloadClubPerformanceStarting district startMonth $ Just $ succ dayOfRecord

download :: CPRS.ClubPerformanceReportSpec -> AppM (Either Text ClubPerformanceReport)
download spec@CPRS.ClubPerformanceReportSpec{CPRS.format, CPRS.programYear} = do
  le <- liftIO $ initLogging "download" "dev"
  let logHeaders req = do
        runKatipContextT le () "http-headers" $ logFM DebugS $ ls $ show req
        pure req
      managerSettings = tlsManagerSettings{managerModifyRequest = logHeaders}
      api = downloadClubPerformanceApi programYear (Just format) (Just spec)
  manager <- liftIO $ newManager managerSettings
  let clientEnv = mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""
  result <- liftIO $ runClientM api clientEnv
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right raw -> do
      runKatipContextT le () "csv" $
        logFM DebugS $
          ls $
            "Downloaded " <> showt format <> " with " <> showt (length (records raw)) <> " records for " <> showt (formatDay (dayOfRecord raw)) <> "."
      pure $ pure raw

reportFromDayOfRecord :: District -> Month -> Day -> AppM (Either Text ClubPerformanceReport)
reportFromDayOfRecord district reportingMonth dayOfRecord = do
  let YearMonth reportingYear reportingMonthOfYear = reportingMonth
      programYear = ProgramYear $ if reportingMonthOfYear < July then pred reportingYear else reportingYear
      spec = CPRS.ClubPerformanceReportSpec CSV district reportingMonth dayOfRecord programYear
      reportingLagDays = diffDays dayOfRecord $ periodLastDay reportingMonth
  if reportingLagDays > 15
    then
      pure $
        Left $
          "Could not get club performance report for "
            <> formatMonth reportingMonth
            <> " by "
            <> formatDay dayOfRecord
            <> "."
    else
      download spec

newtype CsvOctetStream = CsvOctetStream OctetStream
  deriving Accept via OctetStream
instance MimeUnrender CsvOctetStream ClubPerformanceReport where
  mimeUnrender _ bytes = do
    let rows = BL8.lines bytes
    (rawCsv, footer) <-
      maybeToRight ("Could not break " <> show (BL8.length bytes) <> " bytes into lines.") $ unsnoc rows
    (_, parsedCsv) <- decodeByName $ BL8.unlines rawCsv
    (monthReported, dayOfRecord) <- parseFooter $ BL8.unpack footer
    let YearMonth yearOfRecord monthOfRecord = dayPeriod dayOfRecord
        yearReported = if monthReported <= monthOfRecord then yearOfRecord else pred yearOfRecord
        records = toList parsedCsv
    pure ClubPerformanceReport{dayOfRecord, month = YearMonth yearReported monthReported, records}

-- Example footer: "Month of Apr, As of 05/01/2025"
parseFooter :: String -> Either String (MonthOfYear, Day)
parseFooter footer = do
  let (monthPart, dayPart) = break (== ',') footer
      dayOfRecordM = parseTimeM True defaultTimeLocale ", As of %m/%d/%Y" dayPart
      monthM = parseTimeM True defaultTimeLocale "Month of %b" monthPart
  monthOf1970 <-
    maybeToRight
      ("Could not parse month from fragment '" <> monthPart <> "' of CSV footer '" <> footer <> "'.")
      monthM
  let YearMonth _ monthOfYear = monthOf1970
  dayOfRecord <-
    maybeToRight
      ("Could not parse date from fragment '" <> dayPart <> "' of CSV footer '" <> footer <> "'.")
      dayOfRecordM
  pure (monthOfYear, dayOfRecord)
