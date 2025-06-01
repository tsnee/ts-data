{-# LANGUAGE OverloadedStrings #-}

module Logging (initLogging) where

import Katip
import System.IO (stdout)

initLogging :: Namespace -> Environment -> IO LogEnv
initLogging ns env = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V3
  logEnv <- initLogEnv ns env
  registerScribe "stdout" handleScribe defaultScribeSettings logEnv
