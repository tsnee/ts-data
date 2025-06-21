{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Download.Parsers
Description : Pure functions for parsing club performance reports.
Maintainer  : tomsnee@gmail.com
-}
module Download.Parsers (decodeClubReport, parseFooter) where

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv (decodeByName)
import Data.Either.Combinators (maybeToRight)
import Data.List (unsnoc)
import Data.Time (Day (..), MonthOfYear, dayPeriod)
import Data.Time.Calendar.Month (pattern YearMonth)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector (toList)
import Prelude

import Types.ClubPerformanceReport (ClubPerformanceReport (..))

-- | Decode a CSV club performance report.
decodeClubReport :: BL8.ByteString -> Either String ClubPerformanceReport
decodeClubReport bytes = do
  let rows = BL8.lines bytes
  (rawCsv, footer) <-
    maybeToRight
      ("Could not break " <> show (BL8.length bytes) <> " bytes into lines.")
      $ unsnoc rows
  (_, parsedCsv) <- decodeByName $ BL8.unlines rawCsv
  (monthReported, dayOfRecord) <- parseFooter $ BL8.unpack footer
  let YearMonth yearOfRecord monthOfRecord = dayPeriod dayOfRecord
      yearReported = if monthReported <= monthOfRecord then yearOfRecord else pred yearOfRecord
      records = toList parsedCsv
  pure ClubPerformanceReport{dayOfRecord, month = YearMonth yearReported monthReported, records}

-- Example footer: "Month of Apr, As of 05/01/2025"
-- String type mandated by Cassava.
parseFooter :: String -> Either String (MonthOfYear, Day)
parseFooter footer = do
  let (monthPart, dayPart) = break (== ',') footer
      dayOfRecordM = parseTimeM True defaultTimeLocale ", As of %m/%d/%Y" dayPart
      monthM = parseTimeM True defaultTimeLocale "Month of %b" monthPart
  monthOf1970 <-
    maybeToRight
      ("Could not parse month from fragment '" <> monthPart <> "' of CSV footer '" <> footer <> "'.")
      monthM
  let YearMonth _ monthOfYear = monthOf1970
  dayOfRecord <-
    maybeToRight
      ("Could not parse date from fragment '" <> dayPart <> "' of CSV footer '" <> footer <> "'.")
      dayOfRecordM
  pure (monthOfYear, dayOfRecord)
