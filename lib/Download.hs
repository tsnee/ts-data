{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Download (CsvOctetStream (..), download) where

import Prelude

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv (decodeByName)
import Data.Either.Combinators (maybeToRight)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day (..))
import Data.Time.Calendar.Month (Month (..))
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

-- Month of Apr, As of 05/01/2025
parseFooter :: String -> Either String (Month, Day)
parseFooter footer = do
  let (monthPart, dayPart) = break (== ',') footer
      monthM = parseTimeM True defaultTimeLocale "Month of %b" monthPart
      asOfM = parseTimeM True defaultTimeLocale ", As of %m/%d/%C%y" dayPart
  month <-
    maybeToRight
      ("Could not parse month from fragment '" <> monthPart <> "' of CSV footer '" <> footer <> "'.")
      monthM
  asOf <-
    maybeToRight
      ("Could not parse date from fragment '" <> dayPart <> "' of CSV footer '" <> footer <> "'.")
      asOfM
  pure (month, asOf)

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
