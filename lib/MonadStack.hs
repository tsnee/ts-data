{-# LANGUAGE OverloadedStrings #-}

module MonadStack (AppM, runAppM) where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT (..), lift, runReaderT)
import Katip
  ( ColorStrategy (..)
  , Environment
  , KatipContextT
  , LogEnv
  , LogItem (..)
  , Namespace
  , Severity (..)
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
runAppM conf@Conf{env, ns, sev} ctx appM = runResourceT $ do
  (_, logEnv) <- allocate (initLogging env ns sev) $ void . closeScribes
  lift $ flip runReaderT AppEnv{conf, logEnv} $ runKatipContextT logEnv ctx ns appM

initLogging :: Environment -> Namespace -> Severity -> IO LogEnv
initLogging env ns severity = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem severity) V3
  logEnv <- initLogEnv ns env
  registerScribe "stdout" handleScribe defaultScribeSettings logEnv
