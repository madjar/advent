#!/usr/bin/env stack
-- stack runghc --package extra

import Data.List.Extra
import Control.Applicative
import Debug.Trace

data Ingredient = Ingredient { name :: String
                             , capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             } deriving Show

main = interact (show . maximum . map evalMix . mixes 100 . map parseLine . lines)

parseLine :: String -> Ingredient
parseLine l = makeIng name properties
  where [name, props] = splitOn ":" l
        properties = map (read . (!! 1) . splitOn " " . trim) . splitOn "," $ props
        makeIng name [c, d, f, t, ca] = Ingredient name c d f t ca


mixes :: Int -> [Ingredient] -> [[(Int, Ingredient)]]
mixes 0 [] = return []
mixes _ [] = empty
mixes remainingAmount (ingredient:rest) =
  do amount <- [0..remainingAmount]
     otherMixes <- mixes (remainingAmount - amount) rest
     return ((amount, ingredient):otherMixes)

evalMix :: [(Int, Ingredient)] -> Int
evalMix is = if totalCalories == 500
               then product . map (max 0) . foldl (zipWith (+)) [0, 0, 0, 0] . map extractProps $ is
               else -1
  where extractProps (n, i) = map (*n) . map ($i) $ [capacity, durability, flavor, texture]
        totalCalories = sum . map (\(n, i) -> n * calories i) $ is
