module Download.Time (calculatePauseMillis, picosPerMilli) where

secondsPerMinute :: Integer
secondsPerMinute = 60

millisPerSecond :: Integer
millisPerSecond = 1000

picosPerMilli :: Integer
picosPerMilli = 1000000

calculatePauseMillis :: Int -> Integer -> Integer -> Maybe Int
calculatePauseMillis maxRequestsPerMinute startPicos endPicos = do
  let elapsedPicos = endPicos - startPicos
      picosToPause =
        secondsPerMinute * millisPerSecond * picosPerMilli `div` fromIntegral maxRequestsPerMinute
          - elapsedPicos
      millisToPause = picosToPause `div` picosPerMilli
  if millisToPause > 0 then pure (fromInteger millisToPause) else Nothing
