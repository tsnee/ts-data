{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Download.Shell
Description : Impure shell around pure functions for downloading club performance reports.
Maintainer  : tomsnee@gmail.com
-}
module Download.Shell (CsvOctetStream (..), EmptyDayCount, FailureCount, downloadClubPerformanceReportsFrom) where

import Control.Monad.Reader (ask)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T (show)
import Data.Time (Day (..), utctDay)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Katip (Severity (..), logFM, ls, runKatipContextT)
import Network.HTTP.Client (managerModifyRequest, newManager, redirectCount)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Refined (refineTH)
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
import UnliftIO.Concurrent (threadDelay)
import Prelude

import AppM (AppM)
import Download.MealyMachine
  ( MachineConfig (..)
  , MachineInput (..)
  , MachineOutput (..)
  , MachineState (..)
  , initializeMachine
  , step
  )
import Download.MealyMachine qualified as MC (MachineConfig (district))
import Download.Parsers (decodeClubReport)
import Download.Time (calculatePauseMicros)
import PersistenceStore.DbDate (DbDate (..))
import PersistenceStore.SQLite.Insert (saveReport)
import PersistenceStore.SQLite.Query (lookupLastReportDate)
import Types.AppEnv (AppEnv (..))
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..))
import Types.Counts (EmptyDayCount, FailureCount, RequestsPerMinute)
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

-- | Club performance report API
type ClubPerformanceAPI =
  Capture "programYear" ProgramYear
    :> "export.aspx"
    :> QueryParam "type" Format
    :> QueryParam "report" ClubPerformanceReportDescriptor
    :> Get '[CsvOctetStream] ClubPerformanceReport

downloadClubPerformanceReportsFrom
  :: District
  -> Maybe Day
  -> Maybe Day
  -> RequestsPerMinute
  -> EmptyDayCount
  -> FailureCount
  -> AppM ()
downloadClubPerformanceReportsFrom district Nothing endDateM rateLimit maxEmptyDays maxFailures = do
  lastDateM <- lookupLastReportDate
  defaultToToday <- today
  let nextDateM = succ . fromDbDate <$> lastDateM
      startDate = fromMaybe defaultToToday nextDateM
  downloadClubPerformanceReportsFrom
    district
    (Just startDate)
    endDateM
    rateLimit
    maxEmptyDays
    maxFailures
downloadClubPerformanceReportsFrom district startDateM Nothing rateLimit maxEmptyDays maxFailures = do
  endDate <- today
  downloadClubPerformanceReportsFrom
    district
    startDateM
    (Just endDate)
    rateLimit
    maxEmptyDays
    maxFailures
downloadClubPerformanceReportsFrom district (Just startDate) (Just endDate) rateLimit maxEmptyDays maxFailures = do
  servantEnv <- mkServantClientEnv
  let (fsm, actions) =
        initializeMachine $
          MachineConfig
            { MC.district
            , startDate
            , endDate
            , maxEmptyDays
            , emptyDayCount = $$(refineTH 0)
            , maxFailures
            , failureCount = $$(refineTH 0)
            }
  interpret servantEnv rateLimit fsm actions

mkServantClientEnv :: AppM ClientEnv
mkServantClientEnv = do
  AppEnv{logEnv} <- ask
  let logHeaders req = do
        runKatipContextT logEnv () "http-headers" $ logFM DebugS $ ls $ show req
        pure req{redirectCount = 0}
      managerSettings = tlsManagerSettings{managerModifyRequest = logHeaders}
  manager <- liftIO $ newManager managerSettings
  pure $ mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""

interpret :: ClientEnv -> RequestsPerMinute -> MachineState -> [MachineOutput] -> AppM ()
interpret clientEnv rateLimit fsm actions = do
  startPicos <- liftIO getCurrentTime
  traverse_ performAction actions
  case fsm of
    Initial -> logFM ErrorS "Programmer error - Download state machine used incorrectly."
    Failed -> logFM ErrorS "Programmer error - Download state machine given incorrect input."
    Errored -> logFM ErrorS "Too many failures, exiting."
    Finished -> logFM NoticeS "Execution complete."
    Awaiting _ descriptor -> do
      result <- download clientEnv descriptor
      endPicos <- liftIO getCurrentTime
      pause rateLimit startPicos endPicos
      let (nextState, nextActions) = step fsm $ DownloadResult $ first T.show result
      interpret clientEnv rateLimit nextState nextActions

pause :: RequestsPerMinute -> UTCTime -> UTCTime -> AppM ()
pause requestsPerMinute startPicos endPicos =
  case calculatePauseMicros requestsPerMinute startPicos endPicos of
    Nothing ->
      logFM InfoS "No pause necessary."
    Just p -> do
      logFM InfoS $ ls $ "Pausing " <> show p <> " microseconds."
      liftIO $ threadDelay p

performAction :: MachineOutput -> AppM ()
performAction action = do
  case action of
    LogDebug str -> logFM DebugS str
    LogInfo str -> logFM InfoS str
    LogNotice str -> logFM NoticeS str
    LogWarning str -> logFM WarningS str
    LogError str -> logFM ErrorS str
    Save report -> do
      saveReport report
      logFM NoticeS $
        ls $
          mconcat ["Saved ", show (length (records report)), " records for ", show (dayOfRecord report), "."]

newtype CsvOctetStream = CsvOctetStream OctetStream
  deriving Accept via OctetStream

instance MimeUnrender CsvOctetStream ClubPerformanceReport where
  mimeUnrender _ = decodeClubReport

download
  :: ClientEnv
  -> ClubPerformanceReportDescriptor
  -> AppM (Either ClientError ClubPerformanceReport)
download servantEnv spec = liftIO $ runClientM clientM servantEnv
 where
  clientStub = client (Proxy :: Proxy ClubPerformanceAPI)
  clientM = clientStub (programYear spec) (Just (format spec)) $ Just spec

today :: AppM Day
today = liftIO $ utctDay <$> getCurrentTime
