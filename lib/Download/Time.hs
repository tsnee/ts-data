module Download.Time (calculatePauseMicros) where

import Data.Fixed (Pico)
import Data.Time.Clock (UTCTime(..), diffUTCTime, nominalDiffTimeToSeconds)
import Refined (Positive, Refined, unrefine)

secondsPerMinute :: Pico
secondsPerMinute = 60

millisPerSecond :: Pico
millisPerSecond = 1000

microsPerMilli :: Pico
microsPerMilli = 1000

microsPerSecond :: Pico
microsPerSecond = microsPerMilli * millisPerSecond

microsPerMinute :: Pico
microsPerMinute = microsPerSecond * secondsPerMinute

toPico :: Refined Positive Int -> Pico
toPico = fromIntegral . unrefine

calculatePauseMicros :: Refined Positive Int -> UTCTime -> UTCTime -> Maybe Int
calculatePauseMicros maxRequestsPerMinute startPicos endPicos = do
  let rate = toPico maxRequestsPerMinute
      maxMicrosPerRequest = microsPerMinute / rate
      elapsedSeconds = nominalDiffTimeToSeconds $ diffUTCTime endPicos startPicos
      elapsedMicros = elapsedSeconds * microsPerSecond
      microsToPause = floor $ maxMicrosPerRequest - elapsedMicros
  if microsToPause > 0 then pure microsToPause else Nothing
