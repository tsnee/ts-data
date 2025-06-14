{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module PersistenceStore.ClubMetric (ClubMetric (..)) where

import Autodocodec (Autodocodec (..), HasCodec, codec, shownBoundedEnumCodec)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import TextShow (FromStringShow (..), TextShow)
import Prelude

data ClubMetric
  = ActiveMembers
  | Area
  | ClubName
  | ClubStatus
  | DistinguishedStatus
  | District
  | Division
  | DuesOnTimeApril
  | DuesOnTimeOctober
  | GoalsMet
  | HigherLevels
  | LevelOnes
  | LevelThrees
  | LevelTwos
  | MembershipBase
  | NewMembers
  | OfficersListOnTime
  | OfficersTrainedRoundOne
  | OfficersTrainedRoundTwo
  | ReportingMonth
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (FromJSON, ToJSON) via Autodocodec ClubMetric
  deriving TextShow via FromStringShow ClubMetric
instance HasCodec ClubMetric where
  codec = shownBoundedEnumCodec
