{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Download (CsvOctetStream (..), download, downloadClubPerformanceStarting) where

import Prelude

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv (decodeByName)
import Data.Either.Combinators (maybeToRight)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (
  Day (..),
  MonthOfYear,
  addDays,
  dayPeriod,
  diffDays,
  periodFirstDay,
  periodLastDay,
  pattern July,
 )
import Data.Time.Calendar.Month (Month (..), addMonths, pattern YearMonth)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector (toList)
import Network.HTTP.Client (managerModifyRequest, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Accept, Capture, Get, MimeUnrender (..), OctetStream, QueryParam, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import TextShow (printT, showt)

import PersistenceStore.SQLite (saveReport)
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportSpec qualified as CPRS
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

debug :: Bool
debug = False

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

newtype CsvOctetStream = CsvOctetStream OctetStream
  deriving (Accept) via OctetStream
instance MimeUnrender CsvOctetStream ClubPerformanceReport where
  mimeUnrender _ bytes = do
    let rows = BL8.lines bytes
        (rawCsv, footer) = fromMaybe ([], "error") $ unsnoc rows
    (_, parsedCsv) <- decodeByName $ BL8.unlines rawCsv
    (monthReported, dayOfRecord) <- parseFooter $ BL8.unpack footer
    let YearMonth yearOfRecord monthOfRecord = dayPeriod dayOfRecord
        yearReported = if monthReported <= monthOfRecord then yearOfRecord else yearOfRecord - 1
        records = toList parsedCsv
    pure ClubPerformanceReport {dayOfRecord, month = YearMonth yearReported monthReported, records}

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

download :: CPRS.ClubPerformanceReportSpec -> IO (Either Text ClubPerformanceReport)
download clubPerformanceSpec@CPRS.ClubPerformanceReportSpec {CPRS.format, CPRS.programYear} = do
  let logHeaders req = do
        print $ show req
        pure req
      managerSettings =
        if debug
          then tlsManagerSettings {managerModifyRequest = logHeaders}
          else tlsManagerSettings
      api = downloadClubPerformanceApi programYear (Just format) (Just clubPerformanceSpec)
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""
  result <- runClientM api clientEnv
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right raw -> pure $ Right raw

findFirstReportingDate :: District -> Month -> Day -> IO ()
findFirstReportingDate district startMonth dayOfRecord = do
  result <- reportFromDayOfRecord district startMonth dayOfRecord
  case result of
    Left err -> print err
    Right ClubPerformanceReport {records = []} -> findFirstReportingDate district startMonth $ addDays 1 dayOfRecord
    Right report -> do
      saveReport report
      downloadClubPerformanceStarting district startMonth $ Just $ addDays 1 dayOfRecord

(|+|) :: Month -> Integer -> Month
(|+|) = flip addMonths
infixl 6 |+|

downloadClubPerformanceStarting :: District -> Month -> Maybe Day -> IO ()
downloadClubPerformanceStarting district startMonth Nothing = findFirstReportingDate district startMonth $ periodFirstDay startMonth
downloadClubPerformanceStarting district startMonth (Just dayOfRecord) = do
  let showMonth :: Month -> Text
      showMonth = showt . formatTime defaultTimeLocale "%B %Y"
      showDay :: Day -> Text
      showDay = showt . formatTime defaultTimeLocale "%B %d %Y"
  result <- reportFromDayOfRecord district startMonth dayOfRecord
  case result of
    Left err -> print err
    Right ClubPerformanceReport {records = []}
      | dayPeriod dayOfRecord == startMonth |+| 1 ->
          downloadClubPerformanceStarting district (startMonth |+| 1) (Just dayOfRecord)
      | otherwise ->
          printT $ "Could not download data for " <> showMonth startMonth <> " by " <> showDay dayOfRecord
    Right report -> do
      saveReport report
      downloadClubPerformanceStarting district startMonth $ Just $ addDays 1 dayOfRecord

reportFromDayOfRecord :: District -> Month -> Day -> IO (Either Text ClubPerformanceReport)
reportFromDayOfRecord district reportingMonth dayOfRecord = do
  let formatMonth = T.pack . formatTime defaultTimeLocale "%B %Y"
      formatDay = T.pack . formatTime defaultTimeLocale "%B %e, %Y"
      YearMonth reportingYear reportingMonthOfYear = reportingMonth
      programYear = ProgramYear $ if reportingMonthOfYear < July then reportingYear - 1 else reportingYear
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
    else
      download spec
