{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Prelude

import Data.Time (fromGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)

import Download (parseFooter)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then putStrLn $ "✔ " ++ label
    else error $ "✘ " ++ label ++ ": expected " ++ show expected ++ ", got " ++ show actual

main :: IO ()
main = do
  testSimpleScenario
  testYearWrap

testSimpleScenario :: IO ()
testSimpleScenario = do
  let actual = parseFooter "Month of Apr, As of 05/01/2025"
      expectedMonth = YearMonth 2025 4
      expectedDay = fromGregorian 2025 5 1
      expected = Right (expectedMonth, expectedDay)
  assertEqual "Simple scenario" expected actual

testYearWrap :: IO ()
testYearWrap = do
  let actual = parseFooter "Month of Dec, As of 01/01/2025"
      expectedMonth = YearMonth 2024 12
      expectedDay = fromGregorian 2025 1 1
      expected = Right (expectedMonth, expectedDay)
  assertEqual "Previous year" expected actual
