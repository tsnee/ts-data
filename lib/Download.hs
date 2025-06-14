{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Download
Description : Functions for downloading club performance reports.
Maintainer  : tomsnee@gmail.com
-}
module Download
  ( CsvOctetStream (..)
  , DownloadDeps (..)
  , DownloadAction (..)
  , download
  , downloadClubPerformanceStarting
  , downloadClubPerformance
  , parseFooter
  ) where

import Control.Monad.Reader (ask)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv (decodeByName)
import Data.Either.Combinators (maybeToRight)
import Data.List (unsnoc)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T (pack, show)
import Data.Time
  ( Day (..)
  , MonthOfYear
  , dayPeriod
  , diffDays
  , getCurrentTime
  , periodLastDay
  , utctDay
  , pattern July
  )
import Data.Time.Calendar.Month (Month (..), pattern YearMonth)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector (toList)
import Katip (Severity (..), logFM, ls, runKatipContextT)
import Network.HTTP.Client (managerModifyRequest, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Accept, Capture, Get, MimeUnrender (..), OctetStream, QueryParam, (:>))
import Servant.Client
  ( BaseUrl (..)
  , ClientEnv (..)
  , ClientError
  , Scheme (..)
  , client
  , mkClientEnv
  , runClientM
  )
import UnliftIO (liftIO)
import Prelude

import AppM (AppM)
import PersistenceStore.SQLite.Insert (saveReport)
import Types.AppEnv (AppEnv (..))
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..))
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

data DownloadDeps = DownloadDeps
  { fetchReport :: ClubPerformanceReportDescriptor -> AppM (Either Text ClubPerformanceReport)
  , persistReport :: ClubPerformanceReport -> AppM ()
  , currentDay :: AppM Day
  }

data DownloadAction = Continue Day Month | Done

-- | Number of days to try downloading a report before giving up.
maxLagDays :: Integer
maxLagDays = 15

-- | Club performance report API
type ClubPerformanceAPI =
  Capture "programYear" ProgramYear
    :> "export.aspx"
    :> QueryParam "type" Format
    :> QueryParam "report" ClubPerformanceReportDescriptor
    :> Get '[CsvOctetStream] ClubPerformanceReport

downloadClubPerformanceStarting :: District -> Day -> AppM ()
downloadClubPerformanceStarting district dayOfRecord = do
  servantEnv <- mkServantClientEnv
  let deps =
        DownloadDeps
          { fetchReport = download servantEnv
          , persistReport = saveReport
          , currentDay = today
          }
      startMonth = pred $ dayPeriod dayOfRecord
  loop deps dayOfRecord startMonth
 where
  loop deps day month = do
    action <- downloadClubPerformance deps district day month
    case action of
      Done -> pure ()
      Continue nextDay nextMonth -> loop deps nextDay nextMonth

mkServantClientEnv :: AppM ClientEnv
mkServantClientEnv = do
  AppEnv{logEnv} <- ask
  let logHeaders req = do
        runKatipContextT logEnv () "http-headers" $ logFM DebugS $ ls $ show req
        pure req
      managerSettings = tlsManagerSettings{managerModifyRequest = logHeaders}
  manager <- liftIO $ newManager managerSettings
  pure $ mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""

downloadClubPerformance
  :: DownloadDeps
  -> District
  -> Day
  -> Month
  -> AppM DownloadAction
downloadClubPerformance deps district dayOfRecord reportingMonth = do
  let eom = periodLastDay reportingMonth
      lagDays = diffDays dayOfRecord eom
  result <- reportFromDayOfRecord (fetchReport deps) district reportingMonth dayOfRecord
  runDate <- currentDay deps
  case result of
    Left err -> logFM ErrorS (ls err) >> pure Done
    Right ClubPerformanceReport{records = []}
      | dayOfRecord >= runDate -> logFM NoticeS "Done." >> pure Done
      | dayPeriod dayOfRecord == succ reportingMonth -> do
          logFM DebugS $
            ls $
              mconcat
                [ "No records found for "
                , formatMonth reportingMonth
                , " on "
                , formatDay dayOfRecord
                , " - assume month-end reporting completed the previous day."
                ]
          pure $ Continue dayOfRecord (succ reportingMonth)
      | lagDays < maxLagDays -> do
          logFM DebugS $
            ls $
              mconcat
                [ "No records found for "
                , formatMonth reportingMonth
                , " by "
                , formatDay dayOfRecord
                , ", trying next day."
                ]
          pure $ Continue (succ dayOfRecord) reportingMonth
      | otherwise -> do
          logFM WarningS $
            ls $
              mconcat
                [ "No records found for "
                , formatMonth reportingMonth
                , " by "
                , formatDay dayOfRecord
                , ", giving up."
                ]
          pure Done
    Right report -> do
      persistReport deps report
      pure $ Continue (succ dayOfRecord) reportingMonth

reportFromDayOfRecord
  :: (ClubPerformanceReportDescriptor -> AppM (Either Text ClubPerformanceReport))
  -> District
  -> Month
  -> Day
  -> AppM (Either Text ClubPerformanceReport)
reportFromDayOfRecord fetch district reportingMonth dayOfRecord = fetch spec
 where
  YearMonth reportingYear reportingMonthOfYear = reportingMonth
  programYear = ProgramYear $ if reportingMonthOfYear < July then pred reportingYear else reportingYear
  spec = ClubPerformanceReportDescriptor CSV district reportingMonth dayOfRecord programYear

download :: ClientEnv -> ClubPerformanceReportDescriptor -> AppM (Either Text ClubPerformanceReport)
download env spec@ClubPerformanceReportDescriptor{format} = do
  result <- fetchClubPerformanceReport env spec
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right raw -> do
      logFM DebugS $
        ls $
          "Downloaded "
            <> T.show format
            <> " with "
            <> T.show (length (records raw))
            <> " records for "
            <> T.show (formatDay (dayOfRecord raw))
            <> "."
      pure $ pure raw

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
-- String type used by Cassava.
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

fetchClubPerformanceReport
  :: ClientEnv
  -> ClubPerformanceReportDescriptor
  -> AppM (Either ClientError ClubPerformanceReport)
fetchClubPerformanceReport servantEnv spec = liftIO $ runClientM clientM servantEnv
 where
  clientStub = client (Proxy :: Proxy ClubPerformanceAPI)
  clientM = clientStub (programYear spec) (Just (format spec)) $ Just spec

today :: AppM Day
today = liftIO $ utctDay <$> getCurrentTime

formatMonth :: Month -> Text
formatMonth = T.pack . formatTime defaultTimeLocale "%B %Y"

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%B %-d, %Y"
