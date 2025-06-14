{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Types where

import Data.Time (pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Servant.API (toUrlPiece)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..))
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

tests :: TestTree
tests =
  testGroup
    "Types.ClubPerformanceReportDescriptor"
    [ testGroup
        "toUrlPiece"
        [ testCase "District 117 on 5/15/2025" $ do
            let actual =
                  toUrlPiece $
                    ClubPerformanceReportDescriptor
                      CSV
                      (District 117)
                      (YearMonth 2025 5)
                      (YearMonthDay 2025 5 15)
                      (ProgramYear 2024)
                expected = "clubperformance~117~05/31/2025~05/15/2025~2024-2025"
            actual @?= expected
        ]
    ]
