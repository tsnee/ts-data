{-# LANGUAGE OverloadedStrings #-}

module System.AppTestCase (ConnectionAssertion, appConnTestCase, appTestCase) where

import Database.SQLite.Simple (Connection)
import Katip (Namespace, Severity (..), Verbosity (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase)
import Prelude

import AppM (AppM, runAppM)
import PersistenceStore.SQLite.Common (testDatabase, withDatabase)
import Types.Conf (Conf (..))

-- | Assertion for application tests that need database access.
type ConnectionAssertion = Connection -> AppM ()

appTestCase :: Namespace -> TestName -> AppM () -> TestTree
appTestCase ns name assertion =
  testCase name $
    runAppM
      Conf
        { databaseName = testDatabase
        , environment = "test"
        , namespace = ns
        , severity = WarningS
        , verbosity = V3
        }
      ()
      assertion

appConnTestCase :: Namespace -> TestName -> ConnectionAssertion -> TestTree
appConnTestCase ns name assertion =
  appTestCase ns name $ withDatabase assertion
