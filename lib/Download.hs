{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Download (CsvOctetStream (..), download, parseFooter) where

import Prelude

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv (decodeByName)
import Data.Either.Combinators (maybeToRight)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day (..), dayPeriod, periodFirstDay)
import Data.Time.Calendar.Month (Month (..), addMonths, pattern YearMonth)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector (toList)
import Network.HTTP.Client (managerModifyRequest, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Accept, Capture, Get, MimeUnrender (..), OctetStream, QueryParam, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportSpec (ClubPerformanceReportSpec (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

debug :: Bool
debug = False

monthOnOrBefore :: Maybe Day -> Maybe Month -> Maybe Month
monthOnOrBefore dayOfRecordM aboutMonthM = do
  dayOfRecord <- dayOfRecordM
  aboutMonth1970 <- aboutMonthM
  let YearMonth _ ordinalMonth = aboutMonth1970
      aboutMonth = YearMonth (dayPeriod dayOfRecord) (fromIntegral ordinalMonth) -- assume same year
  if periodFirstDay aboutMonth > dayOfRecord
    then
      pure $ addMonths (-12) aboutMonth -- correct bad assumption
    else
      pure aboutMonth

-- Month of Apr, As of 05/01/2025
parseFooter :: String -> Either String (Month, Day)
parseFooter footer = do
  let (monthPart, dayPart) = break (== ',') footer
      dayOfRecordM = parseTimeM True defaultTimeLocale ", As of %m/%d/%Y" dayPart
      monthM = monthOnOrBefore dayOfRecordM $ parseTimeM True defaultTimeLocale "Month of %b" monthPart
  month <-
    maybeToRight
      ("Could not parse month from fragment '" <> monthPart <> "' of CSV footer '" <> footer <> "'.")
      monthM
  dayOfRecord <-
    maybeToRight
      ("Could not parse date from fragment '" <> dayPart <> "' of CSV footer '" <> footer <> "'.")
      dayOfRecordM
  pure (month, dayOfRecord)

newtype CsvOctetStream = CsvOctetStream OctetStream
  deriving (Accept) via OctetStream
instance MimeUnrender CsvOctetStream ((Month, Day), [ClubPerformanceReport]) where
  mimeUnrender _ bytes = do
    let rows = BL8.lines bytes
    let (rawCsv, footer) = fromMaybe ([], "error") $ unsnoc rows
    (_, parsedCsv) <- decodeByName $ BL8.unlines rawCsv
    dates <- parseFooter $ BL8.unpack footer
    pure (dates, toList parsedCsv)

type ClubPerformanceAPI =
  Capture "programYear" ProgramYear
    :> "export.aspx"
    :> QueryParam "type" Format
    :> QueryParam "report" ClubPerformanceReportSpec
    :> Get '[CsvOctetStream] ((Month, Day), [ClubPerformanceReport])

clubPerformanceApi :: Proxy ClubPerformanceAPI
clubPerformanceApi = Proxy

downloadClubPerformanceApi
  :: ProgramYear
  -> Maybe Format
  -> Maybe ClubPerformanceReportSpec
  -> ClientM ((Month, Day), [ClubPerformanceReport])
downloadClubPerformanceApi = client clubPerformanceApi

download :: ClubPerformanceReportSpec -> IO (Either T.Text ((Month, Day), [ClubPerformanceReport]))
download clubPerformance@ClubPerformanceReportSpec {format, programYear} = do
  let logHeaders req = do
        print $ show req
        pure req
      managerSettings =
        if debug
          then
            tlsManagerSettings {managerModifyRequest = logHeaders}
          else
            tlsManagerSettings
      api = downloadClubPerformanceApi programYear (Just format) (Just clubPerformance)
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""
  result <- runClientM api clientEnv
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right raw -> pure $ Right raw
