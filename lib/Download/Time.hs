module Download.Time (calculatePauseMicros) where

import Data.Fixed (Pico)
import Data.Time.Clock -- (NominalDiffTime(..), UTCTime(..), diffUTCTime)

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

calculatePauseMicros :: Int -> UTCTime -> UTCTime -> Maybe Int
calculatePauseMicros maxRequestsPerMinute startPicos endPicos = do
  let elapsedSeconds = nominalDiffTimeToSeconds $ diffUTCTime endPicos startPicos
      elapsedMicros = elapsedSeconds * microsPerSecond
      maxMicrosPerRequest = microsPerMinute / fromIntegral maxRequestsPerMinute
      microsToPause = floor $ maxMicrosPerRequest - elapsedMicros
  if microsToPause > 0 then pure microsToPause else Nothing
