module Types.Conf (Conf (..)) where

import Katip (Environment, Namespace, Severity, Verbosity)

import Types.DatabaseName (DatabaseName (..))

data Conf
  = Conf
  { databaseName :: DatabaseName
  , environment :: Environment
  , namespace :: Namespace
  , severity :: Severity
  , verbosity :: Verbosity
  }
