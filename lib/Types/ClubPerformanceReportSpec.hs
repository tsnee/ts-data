{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReportSpec (ClubPerformanceReportSpec (..)) where

import Data.Text qualified as T
import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (showt)

import Types.District (District)
import Types.Format (Format)
import Types.ProgramYear (ProgramYear)

data ClubPerformanceReportSpec = ClubPerformanceReportSpec
  { format :: !Format
  , district :: !District
  , month :: !T.Text
  , reportedOn :: !T.Text
  , programYear :: !ProgramYear
  }
instance ToHttpApiData ClubPerformanceReportSpec where
  toUrlPiece ClubPerformanceReportSpec {district, month, reportedOn, programYear} =
    T.intercalate
      "~"
      [ "clubperformance"
      , showt district
      , month
      , reportedOn
      , showt programYear
      ]
