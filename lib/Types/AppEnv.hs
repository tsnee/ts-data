module Types.AppEnv (AppEnv (..)) where

import Katip (LogEnv)

import Types.Conf (Conf)

data AppEnv = AppEnv {conf :: Conf, logEnv :: LogEnv}
