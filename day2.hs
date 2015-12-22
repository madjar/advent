#!/usr/bin/env stack
-- stack runghc --package split

import Data.List.Split
import Data.List

main = interact (show . sum . map computeRibbon . map parseLine . lines)

parseLine :: String -> [Int]
parseLine = map read . splitOn "x"

computeArea :: [Int] -> Int
computeArea [a, b, c] = minimum sides + sum sides * 2
  where sides = [a * b, a * c, b * c]

computeRibbon :: [Int] -> Int
computeRibbon dims = ribbon + bow
  where ribbon = (*2) . sum . take 2 $ sort dims
        bow = product dims
