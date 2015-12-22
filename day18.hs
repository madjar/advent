#!/usr/bin/env stack
-- stack runghc --package extra

import Data.Array
import Data.List.Extra

type Grid = Array (Int, Int) Bool

main = interact (show . length . filter id . elems . (!! 100) . iterate (lightCorners . step) . loadInput)

gridBounds = ((0, 0), (99, 99))

loadInput :: String -> Grid
loadInput = listArray gridBounds . map toGrid . filter (/= '\n')
  where toGrid '#' = True
        toGrid '.' = False

fromGrid True = '#'
fromGrid False = '.'

showGrid :: Grid -> String
showGrid = unlines . chunksOf 100 . map fromGrid . elems

step :: Grid -> Grid
step g = listArray gridBounds (map newValue (indices g))
  where newValue i = nextState (g ! i) (countNeighbours i)
        countNeighbours i = length . filter id . map (g !) $ neighbours i
        neighbours :: (Int, Int) -> [(Int, Int)]
        neighbours (i, j) = filter (\x -> x /= (i, j) && inRange gridBounds x)
                                   (range ((i-1, j-1), (i+1, j+1)))
        nextState True n = n == 2 || n == 3
        nextState False n = n == 3

lightCorners :: Grid -> Grid
lightCorners g = g // [(x, True) | x <- [(0, 0), (0, 99), (99, 0), (99, 99)]]
