module PersistenceStore.Analyzer (Analyzer, analyze) where

import Data.List (List)
import Data.Text (Text)
import Data.Time (Day)

import PersistenceStore.Measurement (Measurement (..))
import Types.ClubNumber (ClubNumber)

class Analyzer a where
  analyze :: ClubNumber -> Day -> a -> (List (Measurement Int), List (Measurement Text))
