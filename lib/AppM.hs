{-# LANGUAGE OverloadedStrings #-}

module AppM (AppM, runAppM) where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT (..), lift, runReaderT)
import Katip
  ( ColorStrategy (..)
  , KatipContextT
  , LogEnv
  , LogItem (..)
  , Verbosity (..)
  , closeScribes
  , defaultScribeSettings
  , initLogEnv
  , mkHandleScribe
  , permitItem
  , registerScribe
  , runKatipContextT
  )
import System.IO (stdout)
import UnliftIO.Resource (allocate, runResourceT)
import Prelude

import Types.AppEnv (AppEnv (..))
import Types.Conf (Conf (..))

type AppM = KatipContextT (ReaderT AppEnv IO)

runAppM :: LogItem c => Conf -> c -> AppM a -> IO a
runAppM conf ctx appM = runResourceT $ do
  (_, logEnv) <- allocate (initLogging conf) $ void . closeScribes
  lift $ flip runReaderT AppEnv{conf, logEnv} $ runKatipContextT logEnv ctx (namespace conf) appM

initLogging :: Conf -> IO LogEnv
initLogging Conf{environment, namespace, severity} = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem severity) V3
  logEnv <- initLogEnv namespace environment
  registerScribe "stdout" handleScribe defaultScribeSettings logEnv
