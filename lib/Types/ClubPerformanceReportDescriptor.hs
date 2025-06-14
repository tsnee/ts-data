{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..)) where

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

data ClubPerformanceReportDescriptor = ClubPerformanceReportDescriptor
  { format :: !Format
  , district :: !District
  , reportMonth :: !Month
  , asOf :: !Day
  , programYear :: !ProgramYear
  }
instance ToHttpApiData ClubPerformanceReportDescriptor where
  toUrlPiece ClubPerformanceReportDescriptor{district, reportMonth, asOf, programYear} =
    T.intercalate
      "~"
      [ "clubperformance"
      , showt district
      , showDay $ periodLastDay reportMonth
      , showDay asOf
      , showt programYear
      ]
