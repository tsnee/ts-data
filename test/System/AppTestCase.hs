{-# LANGUAGE OverloadedStrings #-}

module System.AppTestCase (AppAssertion, appTestCase) where

import Database.SQLite.Simple (Connection)
import Katip (Namespace, Severity (..), Verbosity (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase)
import Prelude

import MonadStack (AppM, runAppM)
import PersistenceStore.SQLite.Class (testDatabase, withDatabase)
import Types.Conf (Conf (..))

-- | Assertion for application tests that need database access.
type AppAssertion = Connection -> AppM ()

appTestCase :: Namespace -> TestName -> AppAssertion -> TestTree
appTestCase ns name assertion =
  testCase name $
    runAppM Conf{db = testDatabase, env = "test", ns, sev = WarningS, v = V3} () $
      withDatabase assertion
