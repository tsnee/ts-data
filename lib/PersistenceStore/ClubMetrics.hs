{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module PersistenceStore.ClubMetrics (ClubMetrics (..)) where

import Prelude

import Autodocodec (HasCodec, codec, shownBoundedEnumCodec)
import GHC.Generics (Generic)
import TextShow (FromStringShow (..), TextShow)

data ClubMetrics
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
  deriving (TextShow) via FromStringShow ClubMetrics
instance HasCodec ClubMetrics where
  codec = shownBoundedEnumCodec
