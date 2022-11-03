module Generator where

-- linear congruential pseudo-random number generator
generate :: Int -> [Int]
generate seed = iterate (\ n -> 224149*n + 1) seed