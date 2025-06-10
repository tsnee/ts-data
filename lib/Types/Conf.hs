module Types.Conf (Conf (..)) where

import Katip (Environment, Namespace, Severity, Verbosity)

import Types.DatabaseName (DatabaseName (..))

data Conf = Conf {db :: DatabaseName, env :: Environment, ns :: Namespace, sev :: Severity, v :: Verbosity}
