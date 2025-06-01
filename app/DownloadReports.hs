{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Time.Calendar.Month (pattern YearMonth)

import Download (downloadClubPerformanceStarting)
import MonadStack (runAppM)
import Types.District (District (..))

main :: IO ()
main =
  runAppM "dev" "download-reports" () $
    downloadClubPerformanceStarting (District 117) (YearMonth 2025 5) Nothing
