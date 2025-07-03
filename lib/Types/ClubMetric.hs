{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Types.ClubMetric (ClubMetric (..)) where

import Autodocodec (Autodocodec (..), HasCodec, codec, shownBoundedEnumCodec)
import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic)
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
instance HasCodec ClubMetric where
  codec = shownBoundedEnumCodec
instance ToSchema ClubMetric where
  declareNamedSchema = declareNamedSchemaViaCodec
