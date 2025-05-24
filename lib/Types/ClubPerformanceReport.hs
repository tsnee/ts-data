{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceReport (ClubPerformanceReport (..)) where

import Data.Text qualified as T
import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (showt)

import Types.District (District)
import Types.Format (Format)
import Types.ProgramYear (ProgramYear)

data ClubPerformanceReport = ClubPerformanceReport
    { format :: Format
    , district :: District
    , month :: T.Text
    , reportedOn :: T.Text
    , programYear :: ProgramYear
    }
instance ToHttpApiData ClubPerformanceReport where
    toUrlPiece ClubPerformanceReport{district, month, reportedOn, programYear} =
        T.intercalate
            "~"
            [ "clubperformance"
            , showt district
            , month
            , reportedOn
            , showt programYear
            ]
