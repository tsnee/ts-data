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
module Download (CsvOctetStream (..), download, downloadClubPerformanceStarting, parseFooter) where

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

import MonadStack (AppM)
import PersistenceStore.SQLite.Insert (saveReport)
import Types.AppEnv (AppEnv (..))
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportSpec (ClubPerformanceReportSpec (..))
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

-- | Number of days to try downloading a report before giving up.
maxLagDays :: Integer
maxLagDays = 15

-- | Club performance report API
type ClubPerformanceAPI =
  Capture "programYear" ProgramYear
    :> "export.aspx"
    :> QueryParam "type" Format
    :> QueryParam "report" ClubPerformanceReportSpec
    :> Get '[CsvOctetStream] ClubPerformanceReport

downloadClubPerformanceStarting :: District -> Day -> AppM ()
downloadClubPerformanceStarting district dayOfRecord = do
  servantEnv <- mkServantClientEnv
  downloadClubPerformance servantEnv district dayOfRecord $ pred $ dayPeriod dayOfRecord

mkServantClientEnv :: AppM ClientEnv
mkServantClientEnv = do
  AppEnv{logEnv} <- ask
  let logHeaders req = do
        runKatipContextT logEnv () "http-headers" $ logFM DebugS $ ls $ show req
        pure req
      managerSettings = tlsManagerSettings{managerModifyRequest = logHeaders}
  manager <- liftIO $ newManager managerSettings
  pure $ mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""

downloadClubPerformance :: ClientEnv -> District -> Day -> Month -> AppM ()
downloadClubPerformance servantEnv district dayOfRecord reportingMonth = do
  let eom = periodLastDay reportingMonth
      lagDays = diffDays dayOfRecord eom
  result <- reportFromDayOfRecord servantEnv district reportingMonth dayOfRecord
  runDate <- today
  case result of
    Left err -> logFM ErrorS $ ls err
    Right ClubPerformanceReport{records = []}
      | dayOfRecord >= runDate -> logFM NoticeS "Done."
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
          downloadClubPerformance servantEnv district dayOfRecord $ succ reportingMonth
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
          downloadClubPerformance servantEnv district (succ dayOfRecord) reportingMonth
      | otherwise ->
          logFM WarningS $
            ls $
              mconcat
                [ "No records found for "
                , formatMonth reportingMonth
                , " by "
                , formatDay dayOfRecord
                , ", giving up."
                ]
    Right report -> do
      saveReport report
      downloadClubPerformance servantEnv district (succ dayOfRecord) reportingMonth

reportFromDayOfRecord
  :: ClientEnv -> District -> Month -> Day -> AppM (Either Text ClubPerformanceReport)
reportFromDayOfRecord servantEnv district reportingMonth dayOfRecord = download servantEnv spec
 where
  YearMonth reportingYear reportingMonthOfYear = reportingMonth
  programYear = ProgramYear $ if reportingMonthOfYear < July then pred reportingYear else reportingYear
  spec = ClubPerformanceReportSpec CSV district reportingMonth dayOfRecord programYear

download :: ClientEnv -> ClubPerformanceReportSpec -> AppM (Either Text ClubPerformanceReport)
download env spec@ClubPerformanceReportSpec{format} = do
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
  -> ClubPerformanceReportSpec
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
