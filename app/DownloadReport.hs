{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))

import Download (download)
import PersistenceStore.Class (save)
import PersistenceStore.SQLite.Class (SQLite)
import Types.ClubPerformanceReport (EnhancedClubPerformanceReport (..), MonthWrapper (..))
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
      saveReport = save (Proxy :: Proxy SQLite)
      enhancedReports :: [EnhancedClubPerformanceReport]
      enhancedReports = (\report -> EnhancedClubPerformanceReport report asOf (MonthWrapper month)) <$> reports
