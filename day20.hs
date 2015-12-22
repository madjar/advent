#!/usr/bin/env stack
-- stack runghc

import Data.List
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad

main = undefined

puzzleInput :: Int
puzzleInput = 33100000

-- elvesVisiting :: Int -> [Int]
-- elvesVisiting h = filter (\n -> h `mod` n == 0) [1..h]
--
-- present = (*10) . sum . elvesVisiting
--
-- solution = find (\h -> present h == puzzleInput) [1..]

presents :: Int -> UArray Int Int
presents size = runSTUArray $ do houses <- newListArray (1, size) (repeat 0)
                                 forM_ [1..size] $ \e ->
                                   forM_ (take 50 [e,e+e.. size]) $ \h -> do
                                     current <- readArray houses h
                                     writeArray houses h (current + e * 11)
                                 return houses

solve n = find (\(_, p) -> p >= n) presentsList
  where presentsList = assocs $ presents (n `div` 10)
