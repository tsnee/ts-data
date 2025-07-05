module Unit.Common.Sorting (sortByFirst, sortByDate) where

import Data.Bifunctor (second)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

import Types.ClubMeasurementResponse (Codomain (..), Series (..))

-- | Sort a pair of correlated lists by the first list.
sortByFirst :: [Text] -> [a] -> ([Text], [a])
sortByFirst xs ys = unzip $ (sortBy . comparing) fst $ zip xs ys

-- | Sort the domain and codomain of each 'Series' by its domain values.
sortByDate :: [Series] -> [Series]
sortByDate seriesList = do
  Series{label, domain, codomain} <- seriesList
  let (xs, ys) = case codomain of
        IntCodomain intCodomain -> second IntCodomain $ sortByFirst domain intCodomain
        TextCodomain textCodomain -> second TextCodomain $ sortByFirst domain textCodomain
  pure $ Series label xs ys
