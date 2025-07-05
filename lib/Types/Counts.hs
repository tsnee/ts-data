module Types.Counts (EmptyDayCount, FailureCount, RequestsPerMinute, incr, unlift, (<?), (<=?), (==?), (>=?), (>?)) where

import Refined (NonNegative, Positive, Refined, refine, unrefine)

type EmptyDayCount = Refined NonNegative Int

type FailureCount = Refined NonNegative Int

type RequestsPerMinute = Refined Positive Int

unlift :: Refined Positive Int -> Refined NonNegative Int
unlift x = case refine $ unrefine x of
  Right i -> i
  Left _ -> undefined

(<?) :: Refined p Int -> Refined q Int -> Bool
(<?) x y = unrefine x < unrefine y

(<=?) :: Refined p Int -> Refined q Int -> Bool
(<=?) x y = unrefine x <= unrefine y

(==?) :: Refined p Int -> Refined q Int -> Bool
(==?) x y = unrefine x == unrefine y

(>=?) :: Refined p Int -> Refined q Int -> Bool
(>=?) x y = unrefine x >= unrefine y

(>?) :: Refined p Int -> Refined q Int -> Bool
(>?) x y = unrefine x > unrefine y

class IncrementOps p where
  incr :: Refined p Int -> Refined Positive Int

instance IncrementOps NonNegative where
  incr x = case refine $ succ $ unrefine x of
    Right i -> i
    Left _ -> undefined

instance IncrementOps Positive where
  incr x = case refine $ succ $ unrefine x of
    Right i -> i
    Left _ -> undefined
