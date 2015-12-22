#!/usr/bin/env stack
-- stack runghc --package parsec

import Text.Parsec
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.Maybe


type Aunt = (Int, Things)
type Things = [(String, Int)]


loadAunts :: IO [Aunt]
loadAunts = parseFile input "input16"
loadTape :: IO Things
loadTape = parseFile tape "input16-tape"

input = aunt `sepEndBy` newline <* eof
tape = thing `sepEndBy` newline <* eof
aunt = (,) <$> (string "Sue " *> natural) <* string ": " <*> thing `sepBy` string ", "
thing = (,) <$> many1 letter <* string ": " <*> natural
natural = read <$> many1 digit
parseFile parser f = either (error . show) id . parse parser f <$> readFile f

main = do t <- loadTape
          as <- loadAunts
          print $ filter ( matchesTape t . snd ) as

matchesTape :: Things -> Things -> Bool
matchesTape tape things = all (`matches` tape) things

matches :: (String, Int) -> Things -> Bool
matches (name, n) tape
  | name == "cats" || name == "trees" = n > (fromJust $ lookup name tape)
  | name == "pomeranians" || name == "goldfish" = n < (fromJust $ lookup name tape)
matches t tape = t `elem` tape
