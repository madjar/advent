#!/usr/bin/env stack
-- stack runghc
import qualified Data.Map.Strict as M
import Control.Arrow

main = interact ( show
                . countHousesWithPresent
                . uncurry (M.unionWith (+))
                . (visitHouses *** visitHouses)
                . dispatch)

type Pos = (Int, Int)
type Houses = M.Map Pos Int

visitHouses :: String -> Houses
visitHouses = snd . foldl visit ((0, 0), M.singleton (0, 0) 1)
  where visit :: (Pos, Houses) -> Char -> (Pos, Houses)
        visit (pos, houses) direction = let newPos = follow direction pos
                                        in (newPos, M.insertWith (+) newPos 1 houses)
        follow '^' (x, y) = (x, y+1)
        follow '>' (x, y) = (x+1, y)
        follow '<' (x, y) = (x-1, y)
        follow 'v' (x, y) = (x, y-1)
        follow _ (x, y) = (x, y)

countHousesWithPresent :: Houses -> Int
countHousesWithPresent = length . M.toList

dispatch :: [a] -> ([a], [a])
dispatch [] = ([], [])
dispatch [x] = ([x], [])
dispatch (x: y: xs) = let (xs1, ys1) = dispatch xs in (x:xs1, y:ys1)
