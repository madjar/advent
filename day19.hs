#!/usr/bin/env stack
-- stack runghc --package extra --package heap
{-# LANGUAGE ViewPatterns #-}

import Data.List.Extra
import Data.Maybe
import Control.Applicative
import Debug.Trace
import Data.Tuple
import qualified Data.Heap as H


type Rule = (String, String)

main = interact (show . doit)

doit input = reduction (map swap rules) molecule
  where ls = filter (not . null) . lines $ input
        rules = map (fromJust . stripInfix " => ") . init $ ls
        molecule = last ls

applyRules :: [Rule] -> String -> [String]
applyRules rules mol = nubOrd $ do rule <- rules
                                   applyRule rule mol

applyRule :: Rule -> String -> [String]
applyRule rule@(pattern, result) mol =
  case stripInfix pattern mol of
    Just (before, after) ->     return (before ++ result ++ after)
                            <|> do transformAfter <- applyRule rule after
                                   return (before ++ pattern ++ transformAfter)
    Nothing -> empty

reduction :: [Rule] -> String -> Int
reduction rules start = go (H.singleton (0, (start, 0)))
  where go :: H.MinPrioHeap Int (String, Int) -> Int
        go (H.view -> Just ((_, (mol, level)), rest))
         | traceShow level False = undefined
         | mol == "e" = level
         | otherwise  = go (rest `H.union` newItems)
           where newItems = H.fromList $ map (makeItem (level+1)) $ applyRules rules mol
                 makeItem level mol = (length mol + level, (mol, level))


-- iterateRules :: [Rule] -> String -> [[String]]
-- iterateRules rules mol = [mol] : merge (map (iterateRules rules) (applyRules rules mol))
--   where nextStep = applyRules rules mol
--         merge = map concat . getZipList . sequenceA . map ZipList
--
-- shortest :: [Rule] -> String -> String -> Int
-- shortest rules start goal = depthOf (goal `elem`) tree
--   where tree = iterateRules rules start
--         depthOf p (l:_) | trace "plonk" $ p l =  0
--         depthOf p (_:rest) = 1 + depthOf p rest
