#!/usr/bin/env stack
-- stack runghc --package parsec --package mtl
{-# LANGUAGE LambdaCase, TupleSections #-}

import Text.Parsec hiding (State)
import Control.Monad.State
import Data.List
import Data.Ord
import qualified Data.Map as M
import Data.Map (Map, (!))
import Debug.Trace

data Reindeer = Reindeer { name :: String
                         , speed :: Int
                         , runtime :: Int
                         , rest :: Int
                         } deriving Show

data ReindeerState = Running Int | Resting Int

main = interact (show . maximum . map snd . raceAll 2503 . parseInput)

parseInput = either (error . show) id . parse input ""
  where input = reindeer `sepEndBy` newline <* eof
        reindeer = Reindeer <$> word <* string " can fly "
                            <*> natural <* string " km/s for "
                            <*> natural <* string " seconds, but then must rest for "
                            <*> natural <* string " seconds."
        word = many1 letter
        natural = read <$> many1 digit

step :: Reindeer -> State ReindeerState Int
step r = do modify $ \case
              Running 0 -> Resting (rest r - 1)
              Running n -> Running (n-1)
              Resting 0 -> Running (runtime r - 1)
              Resting n -> Resting (n-1)
            get >>= \case Running _ -> return (speed r)
                          _         -> return 0


race :: Int -> Reindeer -> [Int]
race n r = evalState run (Resting 0)
  where run =  do times <- mapM (const $ step r) [1..n]
                  return $ scanl (+) 0 times


raceAll :: Int -> [Reindeer] -> [(Int, Int)]
raceAll n rs = M.toList . counter . concatMap winners $ seconds
  where positions = map (race n) rs
        seconds = drop 1 (transpose positions)
        winners :: [Int] -> [Int]  --list of winners
        winners second = let leadPosition = maximum second
                         in map snd . filter ((leadPosition ==) . fst) $ zip second [0..]


counter :: Ord a => [a] -> Map a Int
counter = M.fromListWith (+) . map (,1)
