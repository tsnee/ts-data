{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unit.Libs.Types.ClubPerformanceReportDescriptor where

import Data.Time (pattern YearMonthDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Servant.API (toUrlPiece)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Types.ClubPerformanceReportDescriptor (ClubPerformanceReportDescriptor (..))
import Types.District (District (..))
import Types.Format (Format (..))
import Types.ProgramYear (ProgramYear (..))

tests :: TestTree
tests =
  testGroup
    "Unit.Libs.Types.ClubPerformanceReportDescriptor"
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
            expected @?= actual
        ]
    ]
