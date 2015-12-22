#!/usr/bin/env stack
-- stack runghc  --package parsec

import Data.List
import Text.Parsec
import qualified Data.Map as M
import Data.Map (Map)
import Control.Arrow
import Data.Tuple
import qualified Data.Set as S


main = interact (show . maximum . routeDistances . parseDistances)

parseDistances :: String -> Map (String, String) Int
parseDistances = M.fromList . reverseCities . parseInput
  where parseInput = either (error . show) id . parse input ""
        input = line `sepEndBy` newline <* eof
        line = (,) <$> ((,) <$> city <* string " to " <*> city) <* string " = " <*> natural
        city = many1 letter
        natural = read <$> many1 digit
        reverseCities l = l ++ map (first swap) l

routeDistances dist = map computeDistance possibleRoutes
  where cities = S.toList . S.fromList . map (fst.fst) . M.toList $ dist
        possibleRoutes = permutations cities
        computeDistance = sum . map (dist M.!) . (zip <*> tail)
