#!/usr/bin/env stack
-- stack runghc --package parsec

import Text.Parsec
import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Set as S
import Data.List
import Debug.Trace

main = interact (show . findSolution . parseInput)

parseInput :: String -> Map (String, String) Int
parseInput = M.fromList . either (error . show) id . parse input ""
  where input = line `sepEndBy` newline <* eof
        line = do person1 <- person
                  string " would "
                  score <- happiness
                  string " by sitting next to "
                  person2 <- person
                  string "."
                  return ((person1, person2), score)
        person = many1 letter
        happiness = verb <* string " " <*> number <* string " happiness units"
        verb = string "gain" *> return id <|> string "lose" *> return negate
        number = read <$> many1 digit

findSolution :: Map (String, String) Int -> Int
findSolution oldRules = maximum $ map evaluateTable possibleTables
  where persons = S.toList . S.fromList . map (fst.fst) . M.toList $ oldRules
        rules = M.union oldRules $ M.fromList (concatMap (\p ->[(("me", p), 0), ((p, "me"), 0)]) persons)
        possibleTables = permutations ("me": persons)
        evaluateTable t = sum . map happynessForPair . (zip <*> tail) $ (t ++ [head t])
        happynessForPair (p1, p2) = rules ! (p1, p2) + rules ! (p2, p1)
