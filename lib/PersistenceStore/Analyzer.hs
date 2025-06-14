module PersistenceStore.Analyzer (Analyzer, analyze) where

import Data.Time (Day)

import PersistenceStore.Measurement (Measurement (..))
import Types.ClubNumber (ClubNumber)

class Analyzer a b where
  analyze :: ClubNumber -> Day -> a -> [Measurement b]
