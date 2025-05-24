{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable               (traverse_)
import           Data.Proxy                  (Proxy(..))

import           Download                    (download)
import           PersistenceStore.Class      (save)
import           PersistenceStore.SQLite     (SQLite)
import           Types.District              (District (..))
import           Types.ClubPerformanceReport (ClubPerformanceReport (..))
import           Types.ClubPerformanceResult (EnhancedClubPerformanceResult(..))
import           Types.Format                (Format(..))
import           Types.ProgramYear           (ProgramYear (..))

main :: IO ()
main = do
  result <- download (ClubPerformanceReport CSV (District 117) "4/30/2025" "5/1/2025" (ProgramYear 2024))
  case result of
    Left err -> print err
    Right ((month, asOf), reports) ->
      traverse_ saveReport enhancedReports where
        saveReport :: EnhancedClubPerformanceResult -> IO ()
        saveReport = save (Proxy :: Proxy SQLite)
        enhancedReports :: [EnhancedClubPerformanceResult]
        enhancedReports = (\report -> EnhancedClubPerformanceResult report asOf month) <$> reports
