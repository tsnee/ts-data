module PersistenceStore.Analyzer (Analyzer, analyze) where

import Data.List (List)
import Data.Text (Text)
import Data.Time (Day)

import PersistenceStore.MetricValueRow (MetricValueRow (..))

import Types.ClubNumber (ClubNumber)

class Analyzer a where
  analyze :: ClubNumber -> Day -> a -> (List (MetricValueRow Int), List (MetricValueRow Text))
