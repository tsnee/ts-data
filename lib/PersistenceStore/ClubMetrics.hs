{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module PersistenceStore.ClubMetrics (ClubMetric (..)) where

import Autodocodec (HasCodec, codec, shownBoundedEnumCodec)
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
  deriving TextShow via FromStringShow ClubMetric
instance HasCodec ClubMetric where
  codec = shownBoundedEnumCodec
