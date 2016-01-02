#!/usr/bin/env stack
-- stack runghc

fromGrid :: Int -> Int-> Int
fromGrid row column = diagBase + column
  where diag = row + column - 1
        diagBase = diag * (diag - 1) `div` 2

answer = codes !! (fromGrid 2981 3075 - 1)

codes = iterate f 20151125
  where f i = i * 252533 `mod` 33554393

main = print answer
