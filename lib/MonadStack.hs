module MonadStack (AppM, runAppM) where

import Katip (Environment, KatipContextT, LogItem (..), Namespace, runKatipContextT)
import Prelude

import Logging (initLogging)

type AppM = KatipContextT IO

runAppM :: LogItem c => Environment -> Namespace -> c -> AppM a -> IO a
runAppM env ns ctx appM = do
  logEnv <- initLogging ns env
  runKatipContextT logEnv ctx ns appM
