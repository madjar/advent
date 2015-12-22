#!/usr/bin/env stack
-- stack runghc -- package extra

import Extra
import Data.List
import Data.Maybe

current = "cqjxjnds"


solve = fromJust . find goodPass . iterate increment . increment


increment :: String -> String
increment = snd . foldr f (True, [])
  where f c (carry, s) = if carry
                           then let (newCarry, c2) = next c in (newCarry, c2 : s)
                           else (False, c:s)
        next 'z' = (True, 'a')
        next c = (False, succ c)

goodPass = firstReq &&^ secondReq &&^ thirdReq

firstReq pass = any (`isInfixOf` pass) straights
  where straights = slidingWindow 3 ['a'..'z']

secondReq pass = null (pass `intersect` forbidden)
  where forbidden = "iol"

thirdReq :: String -> Bool
thirdReq = (>= 2) . length . nub . filter isPair . slidingWindow 2
  where isPair [x, y] | x == y = True
        isPair _ = False

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = dropEnd n . map (take n) . tails
