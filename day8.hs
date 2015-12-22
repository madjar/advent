#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Text (pack, count, breakOnAll, replace)

import Debug.Trace

main = interact ( show . sum . map (countAll2 . pack) . lines)

countAll s = countSlash + countRest
  where countSlash = (length $ breakOnAll "\\\\" s)
        unslashed = replace "\\\\" "$" s
        countRest =  3 * count "\\x" unslashed + count "\"" unslashed

countAll2 s = count "\"" s + count "\\" s + 2
