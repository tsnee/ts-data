{-# LANGUAGE OverloadedStrings #-}

module Logging (initLogging) where

import Katip
import System.IO (stdout)

initLogging :: Namespace -> Environment -> Severity -> IO LogEnv
initLogging ns env severity = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem severity) V3
  logEnv <- initLogEnv ns env
  registerScribe "stdout" handleScribe defaultScribeSettings logEnv
