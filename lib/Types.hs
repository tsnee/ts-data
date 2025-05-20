{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
  Area(..),
  ClubPerformanceReport(..),
  ClubPerformanceResult(..),
  ClubStatus(..),
  DistinguishedStatus(..),
  District(..),
  Division(..),
  Format(..),
  ProgramYear(..)
) where

import Prelude

import qualified Data.ByteString.Char8 as BS
import Data.Csv (FromField, FromNamedRecord, parseField, parseNamedRecord, (.:))
import qualified Data.Text as T
import Servant.API (ToHttpApiData, toUrlPiece)
import TextShow (TextShow, fromString, showb, showt)

newtype Area = Area Int
  deriving (FromField, Show)

data ClubStatus = Active | Low | Suspended
  deriving Show
instance FromField ClubStatus where
  parseField f = case BS.unpack f of
    "Active" -> pure Active
    "Low" -> pure Low
    "Suspended" -> pure Suspended
    _ -> pure Suspended

data DistinguishedStatus = Smedly | Presidents | Select | Distinguished | NotYet
  deriving Show
instance FromField DistinguishedStatus where
  parseField f = case BS.unpack f of
    "Presidents" -> pure Presidents
    "Select" -> pure Select
    "Distinguished" -> pure Distinguished
    _ -> pure NotYet

newtype District = District Int
  deriving (FromField, Show)
instance TextShow District where
  showb (District i) = showb i

newtype Division = Divsion Char
  deriving (FromField, Show)

data Format = CSV | PDF
  deriving Show
instance TextShow Format where
  showb = fromString . show
instance ToHttpApiData Format where
  toUrlPiece = showt

newtype ProgramYear = ProgramYear Int
instance TextShow ProgramYear where
  showb (ProgramYear year) = showb year <> fromString "-" <> showb (year + 1)
instance ToHttpApiData ProgramYear where
  toUrlPiece = showt

data ClubPerformanceReport = ClubPerformanceReport
  { format :: Format
  , district :: District
  , month :: T.Text
  , reportedOn :: T.Text
  , programYear :: ProgramYear
  }
instance ToHttpApiData ClubPerformanceReport where
  toUrlPiece ClubPerformanceReport { district, month, reportedOn, programYear } = T.intercalate "~"
    [ "clubperformance"
    , showt district
    , month
    , reportedOn
    , showt programYear
    ]

data ClubPerformanceResult = ClubPerformanceResult
  { dist   :: District
  , div   :: Division
  , area       :: Area
  , clubNumber :: Int
  , clubName   :: T.Text
  , clubStatus :: ClubStatus
  , membershipBase :: Int
  , activeMembers  :: Int
  , goalsMet :: Int
  , level1s :: Int
  , level2s :: Int
  , moreLevel2s :: Int
  , level3s :: Int
  , level4s5sOrDtms :: Int
  , moreLevel4s5sOrDtms :: Int
  , newMembers :: Int
  , moreNewMembers :: Int
  , summerOfficersTrained :: Int
  , winterOfficersTrained :: Int
  , duesPaidOctober :: Int
  , duesPaidApril :: Int
  , officerListOnTime :: Int
  , distinguishedStatus :: DistinguishedStatus
  } deriving Show
instance FromNamedRecord ClubPerformanceResult where
  parseNamedRecord r = ClubPerformanceResult
    <$> r .: "District"
    <*> r .: "Division"
    <*> r .: "Area"
    <*> r .: "Club Number"
    <*> r .: "Club Name"
    <*> r .: "Club Status"
    <*> r .: "Mem. Base"
    <*> r .: "Active Members"
    <*> r .: "Goals Met"
    <*> r .: "Level 1s"
    <*> r .: "Level 2s"
    <*> r .: "Add. Level 2s"
    <*> r .: "Level 3s"
    <*> r .: "Level 4s, Level 5s, or DTM award"
    <*> r .: "Add. Level 4s, Level 5s, or DTM award"
    <*> r .: "New Members"
    <*> r .: "Add. New Members"
    <*> r .: "Off. Trained Round 1"
    <*> r .: "Off. Trained Round 2"
    <*> r .: "Mem. dues on time Oct"
    <*> r .: "Mem. dues on time Apr"
    <*> r .: "Off. List On Time"
    <*> r .: "Club Distinguished Status"
