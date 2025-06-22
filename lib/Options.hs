module Options
  ( confParser
  , parseConf
  , parseWithConf
  ) where

import Data.String (fromString)
import Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , maybeReader
  , metavar
  , option
  , short
  , showDefaultWith
  , str
  , value
  , (<**>)
  )
import Text.Read (readMaybe)

import Types.Conf (Conf (..))
import Types.DatabaseName (DatabaseName (..))

confParser :: Conf -> Parser Conf
confParser
  Conf
    { databaseName = DatabaseName dbDef
    , environment = envDef
    , namespace = nsDef
    , severity = sevDef
    , verbosity = verbDef
    } =
    Conf
      <$> option
        (DatabaseName <$> str)
        ( short 'D'
            <> long "database-name"
            <> metavar "FILE"
            <> help "SQLite database file"
            <> value (DatabaseName dbDef)
            <> showDefaultWith show
        )
      <*> option
        (fromString <$> str)
        ( short 'E'
            <> long "environment"
            <> metavar "ENV"
            <> help "Application environment"
            <> value envDef
            <> showDefaultWith show
        )
      <*> option
        (fromString <$> str)
        ( short 'N'
            <> long "namespace"
            <> metavar "NS"
            <> help "Logging namespace"
            <> value nsDef
            <> showDefaultWith show
        )
      <*> option
        readSeverity
        ( short 'S'
            <> long "severity"
            <> metavar "SEVERITY"
            <> help "Minimum log severity"
            <> value sevDef
            <> showDefaultWith show
        )
      <*> option
        readVerbosity
        ( short 'V'
            <> long "verbosity"
            <> metavar "VERBOSITY"
            <> help "Log verbosity"
            <> value verbDef
            <> showDefaultWith show
        )
   where
    readSeverity = maybeReader readMaybe
    readVerbosity = maybeReader readMaybe

parseConf :: Conf -> IO Conf
parseConf def = execParser $ info (confParser def <**> helper) fullDesc

parseWithConf :: Conf -> Parser a -> IO (Conf, a)
parseWithConf def extra =
  execParser $ info ((,) <$> confParser def <*> extra <**> helper) fullDesc
