#!/usr/bin/env stack
-- stack runghc --package lens --package extra
{-# LANGUAGE PartialTypeSignatures #-}

import Data.List.Extra
import Control.Lens
import Control.Monad
import Debug.Trace
import Control.Applicative

main = interact (show . solve . map read . lines)


solve :: [Int] -> Int
solve = minimum . map product . head . groupOn length . mkGroups . reverse

mkGroups :: [Int] -> [[Int]]
mkGroups presents = findFirstGroup presents
  where findGroup ps = do g <- filter isGood . orderedSubsequences $ ps
                          return (g, ps \\ g)
        findSecondGroup ps = do (g, r) <- findGroup ps
                                guard (any isGood (subsequences r))
                                return g
        findFirstGroup ps = do (g, r) <- findGroup ps
                               guard (not . null $ findSecondGroup r)
                               return g
        isGood g = sum g == weight
        weight = sum presents `div` 4

addoneall x xs = zipWith ((++) . (map (x:))) ([]:xs) (xs ++ [[]])
orderedSubsequences = concat . foldr addoneall [[[]]]
