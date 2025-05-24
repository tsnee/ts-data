{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ClubPerformanceResult (ClubPerformanceResult(..), EnhancedClubPerformanceResult(..)) where

import           Prelude

import           Data.Csv                         (FromNamedRecord, parseNamedRecord, (.:))
import qualified Data.Text                        as T
import           Data.Time                        (Day)
import           Data.Time.Calendar.Month         (Month(..))
import           Database.SQLite.Simple           (FromRow(..), ToRow(..))
import           Database.SQLite.Simple.FromField (fromField)
import           Database.SQLite.Simple.ToField   (toField)
import           GHC.Generics                     (Generic)

import           Types.Area                       (Area)
import           Types.ClubStatus                 (ClubStatus)
import           Types.DistinguishedStatus        (DistinguishedStatus)
import           Types.District                   (District)
import           Types.Division                   (Division)

data ClubPerformanceResult = ClubPerformanceResult
  { dist                  :: District
  , div                   :: Division
  , area                  :: Area
  , clubNumber            :: Int
  , clubName              :: T.Text
  , clubStatus            :: ClubStatus
  , membershipBase        :: Int
  , activeMembers         :: Int
  , goalsMet              :: Int
  , level1s               :: Int
  , level2s               :: Int
  , moreLevel2s           :: Int
  , level3s               :: Int
  , level4s5sOrDtms       :: Int
  , moreLevel4s5sOrDtms   :: Int
  , newMembers            :: Int
  , moreNewMembers        :: Int
  , summerOfficersTrained :: Int
  , winterOfficersTrained :: Int
  , duesPaidOctober       :: Int
  , duesPaidApril         :: Int
  , officerListOnTime     :: Int
  , distinguishedStatus   :: DistinguishedStatus
  } deriving (Generic, Show)
instance FromRow ClubPerformanceResult
instance ToRow ClubPerformanceResult
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

data EnhancedClubPerformanceResult = EnhancedClubPerformanceResult
  { cpr   :: ClubPerformanceResult
  , asOf  :: Day
  , month :: Month
  } deriving Generic
instance FromRow EnhancedClubPerformanceResult where
  fromRow = undefined
instance ToRow EnhancedClubPerformanceResult where
  toRow EnhancedClubPerformanceResult { cpr, asOf, month } = toRow cpr <> [toField asOf] <> [toField n] where
    n = case month of
      MkMonth m -> m
