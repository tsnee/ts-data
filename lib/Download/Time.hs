module Download.Time (calculatePauseMicros) where

secondsPerMinute :: Integer
secondsPerMinute = 60

microsPerSecond :: Integer
microsPerSecond = 1000000

picosPerMicro :: Integer
picosPerMicro = 1000

calculatePauseMicros :: Int -> Integer -> Integer -> Maybe Int
calculatePauseMicros maxRequestsPerMinute startPicos endPicos = do
  let elapsedPicos = endPicos - startPicos
      picosToPause =
        ((secondsPerMinute * microsPerSecond * picosPerMicro) `div` fromIntegral maxRequestsPerMinute)
          - elapsedPicos
      millisToPause = picosToPause `div` picosPerMicro
  if millisToPause > 0 then pure (fromInteger millisToPause) else Nothing
