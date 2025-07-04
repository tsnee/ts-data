{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Download.Shell
Description : Impure shell around pure functions for downloading club performance reports.
Maintainer  : tomsnee@gmail.com
-}
module Download.Shell (CsvOctetStream (..), downloadClubPerformanceReportsFrom) where

import Control.Monad.Reader (ask)
import Data.Bifunctor (first)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T (show)
import Data.Time (Day (..), utctDay)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Katip (Severity (..), logFM, ls, runKatipContextT)
import Network.HTTP.Client (managerModifyRequest, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Refined (Positive, Refined)
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
import PersistenceStore.SQLite.Insert (saveReport)
import Types.AppEnv (AppEnv (..))
import Types.ClubPerformanceReport (ClubPerformanceReport (..))
import Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..))
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

downloadClubPerformanceReportsFrom :: District -> Day -> Maybe Day -> Refined Positive Int -> Int -> AppM ()
downloadClubPerformanceReportsFrom district startDate Nothing rateLimit maxFailures = do
  endDate <- today
  downloadClubPerformanceReportsFrom district startDate (Just endDate) rateLimit maxFailures
downloadClubPerformanceReportsFrom district startDate (Just endDate) rateLimit maxFailures = do
  servantEnv <- mkServantClientEnv
  let (fsm, actions) = initializeMachine $ MachineConfig{MC.district, startDate, endDate, maxFailures, failureCount = 0}
  interpret servantEnv rateLimit fsm actions

mkServantClientEnv :: AppM ClientEnv
mkServantClientEnv = do
  AppEnv{logEnv} <- ask
  let logHeaders req = do
        runKatipContextT logEnv () "http-headers" $ logFM DebugS $ ls $ show req
        pure req
      managerSettings = tlsManagerSettings{managerModifyRequest = logHeaders}
  manager <- liftIO $ newManager managerSettings
  pure $ mkClientEnv manager $ BaseUrl Https "dashboards.toastmasters.org" 443 ""

interpret :: ClientEnv -> Refined Positive Int -> MachineState -> [MachineOutput] -> AppM ()
interpret clientEnv rateLimit fsm actions = do
  startPicos <- liftIO getCurrentTime
  sequence_ $ performActions actions
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

pause :: Refined Positive Int -> UTCTime -> UTCTime -> AppM ()
pause requestsPerMinute startPicos endPicos =
  case calculatePauseMicros requestsPerMinute startPicos endPicos of
    Nothing ->
      logFM InfoS "No pause necessary."
    Just p -> do
      logFM InfoS $ ls $ "Pausing " <> show p <> " microseconds."
      liftIO $ threadDelay p

performActions :: [MachineOutput] -> [AppM ()]
performActions actions = do
  action <- actions
  case action of
    LogDebug str -> pure $ logFM DebugS str
    LogInfo str -> pure $ logFM InfoS str
    LogNotice str -> pure $ logFM NoticeS str
    LogWarning str -> pure $ logFM WarningS str
    LogError str -> pure $ logFM ErrorS str
    Save report -> pure $ do
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
