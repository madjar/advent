#!/usr/bin/env stack
-- stack runghc

import Data.List

main = interact (show . solve . map read . lines)

solve :: [Int] -> Int
solve l = length . filter ((== minSize) . length) $ solutions
  where solutions = filter ((== 150) . sum) . subsequences $ l
        minSize = minimum . map length $ solutions
