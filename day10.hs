#!/usr/bin/env stack
-- stack runghc

import Data.List

input = "1113122113"

lookAndSay = concatMap say . group
  where say g = show (length g) ++ take 1 g

result = length (iterate lookAndSay input !! 40)
