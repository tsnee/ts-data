{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import TextShow (printT)

import Download (download)
import PersistenceStore.Class (save)
import PersistenceStore.SQLite (SQLite)
import Types.ClubPerformanceReport (EnhancedClubPerformanceReport (..))
import Types.ClubPerformanceReportSpec (ClubPerformanceReportSpec (..))
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

main :: IO ()
main = do
  result <-
    download (ClubPerformanceReportSpec CSV (District 117) "4/30/2025" "5/1/2025" (ProgramYear 2024))
  case result of
    Left err -> print err
    Right ((month, asOf), reports) ->
      traverse_ saveReport enhancedReports
     where
      saveReport :: EnhancedClubPerformanceReport -> IO ()
      saveReport r = do
        printT r
        save (Proxy :: Proxy SQLite) r
      enhancedReports :: [EnhancedClubPerformanceReport]
      enhancedReports = (\report -> EnhancedClubPerformanceReport report asOf month) <$> reports
