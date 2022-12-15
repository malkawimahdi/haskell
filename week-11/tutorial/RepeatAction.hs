module RepeatAction where

repeat :: Int -> IO a -> IO()
repeat amount action = sequence_ $ replicate amount action