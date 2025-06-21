{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..)) where

import Data.Text (Text)
import Data.Text qualified as T (intercalate, pack)
import Data.Time (Day (..), defaultTimeLocale, formatTime, periodLastDay)
import Data.Time.Calendar.Month (Month (..))
import Servant.API (ToHttpApiData, toUrlPiece)

import Types.District (District)
import Types.Format (Format)
import Types.ProgramYear (ProgramYear)

dayToUrlPiece :: Day -> Text
dayToUrlPiece = T.pack . formatTime defaultTimeLocale "%m/%d/%Y"

data ClubPerformanceReportDescriptor = ClubPerformanceReportDescriptor
  { format :: !Format
  , district :: !District
  , reportMonth :: !Month
  , asOf :: !Day
  , programYear :: !ProgramYear
  }
  deriving Show
instance ToHttpApiData ClubPerformanceReportDescriptor where
  toUrlPiece ClubPerformanceReportDescriptor{district, reportMonth, asOf, programYear} =
    T.intercalate
      "~"
      [ "clubperformance"
      , toUrlPiece district
      , dayToUrlPiece $ periodLastDay reportMonth
      , dayToUrlPiece asOf
      , toUrlPiece programYear
      ]
