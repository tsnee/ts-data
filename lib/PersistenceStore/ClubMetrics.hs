module PersistenceStore.ClubMetrics (ClubMetrics (..)) where

import Prelude

import TextShow (TextShow, fromString, showb)

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
instance TextShow ClubMetrics where
  showb = fromString . show
