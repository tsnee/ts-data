{-# LANGUAGE DerivingVia #-}

module PersistenceStore.ClubMetrics (ClubMetrics (..)) where

import Prelude

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
  deriving (Bounded, Enum, Eq, Show)
  deriving (TextShow) via FromStringShow ClubMetrics
