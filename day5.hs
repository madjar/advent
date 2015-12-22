#!/usr/bin/env stack
-- stack runghc --package extra
import Extra
import Data.List

main = interact (show . length . filter isNicer . lines)

isNice :: String -> Bool
isNice = threeVowels &&^ twoLettersInARow &&^ noBadString

threeVowels :: String -> Bool
threeVowels s = length (filter (`elem` "aeiou") s) >= 3

twoLettersInARow :: String -> Bool
twoLettersInARow = any (uncurry (==)) . (zip <*> tail)

noBadString :: String -> Bool
noBadString s = not (any (`isInfixOf` s) baddies)
  where baddies = ["ab", "cd", "pq", "xy"]

isNicer :: String -> Bool
isNicer = repeatedPair &&^ repeatWithLetterInBetween

repeatedPair :: String -> Bool
repeatedPair = any isNicePair . tails
  where isNicePair (x:y:rest) = [x, y] `isInfixOf` rest
        isNicePair _ = False

repeatWithLetterInBetween :: String -> Bool
repeatWithLetterInBetween = any isNiceRepetition . (zip3 <*> tail <*> tail.tail)
  where isNiceRepetition (x, _, y) | x == y = True
        isNiceRepetition _ = False
