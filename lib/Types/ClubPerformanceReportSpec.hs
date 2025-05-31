{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReportSpec (ClubPerformanceReportSpec (..)) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day (..), defaultTimeLocale, formatTime, periodLastDay)
import Data.Time.Calendar.Month (Month (..))
import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (showt)

import Types.District (District)
import Types.Format (Format)
import Types.ProgramYear (ProgramYear)

showDay :: Day -> Text
showDay = T.pack . formatTime defaultTimeLocale "%m/%d/%Y"

data ClubPerformanceReportSpec = ClubPerformanceReportSpec
  { format :: !Format
  , district :: !District
  , month :: !Month
  , reportedOn :: !Day
  , programYear :: !ProgramYear
  }
instance ToHttpApiData ClubPerformanceReportSpec where
  toUrlPiece ClubPerformanceReportSpec{district, month, reportedOn, programYear} =
    T.intercalate
      "~"
      [ "clubperformance"
      , showt district
      , showDay $ periodLastDay month
      , showDay reportedOn
      , showt programYear
      ]
