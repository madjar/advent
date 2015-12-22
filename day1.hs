#!/usr/bin/env stack
-- stack runghc
import Data.List


main = interact countParens

-- countParens :: String -> String
-- countParens = show . sum . map value
--   where value '(' = 1
--         value ')' = -1
--         value _   = 0

countParens :: String -> String
countParens = show . findIndex (== -1) . scanl (+) 0 . map value
  where value '(' = 1
        value ')' = -1
        value _   = 0
