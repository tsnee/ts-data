{-# LANGUAGE OverloadedStrings #-}

module MonadStack (AppM, runAppM, testAppM) where

import Katip (Environment, KatipContextT, LogItem (..), Namespace, Severity (..), runKatipContextT)
import Prelude

import Logging (initLogging)

type AppM = KatipContextT IO

runAppM :: LogItem c => Environment -> Namespace -> c -> Severity -> AppM a -> IO a
runAppM env ns ctx severity appM = do
  logEnv <- initLogging ns env severity
  runKatipContextT logEnv ctx ns appM

testAppM :: LogItem c => c -> AppM a -> IO a
testAppM ctx appM = do
  logEnv <- initLogging "system-test" "test" WarningS
  runKatipContextT logEnv ctx "system-test" appM
